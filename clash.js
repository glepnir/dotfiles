// Define main function (script entry)

const staticProxy = {
  type: "socks5", // 支持 http 或 socks5
  server: "158.94.106.95",
  port: 18084,
  username: "hLfypP6AGX",
  password: "A2piwBDDDL",
};

const staticGroup = "🔒 静态出口自动选择";
const chainPrefix = "🔗 静态链路 · ";
// 优先使用的前置节点（名称包含即可），其后依次是其他美国节点，最后是其他地区节点
const pinnedNode = "US美国 04A";
// 信息类节点和静态代理自身，不生成链路
const infoRe = /剩余|到期|官网|账号|SOCKS5/;

const staticDomains = [
  "x.com", "twitter.com", "twimg.com", "t.co",
  "twitteroauth.com", "twtrdns.net", "twttr.com", "twttr.net",
  "twvid.com", "openai.com", "chatgpt.com", "oaistatic.com",
  "oaiusercontent.com", "oaistatsig.com", "openaimerge.com", "workos.com",
  "workoscdn.com", "authkit.app", "claude.ai", "anthropic.com",
  "claude.com", "claude.dev", "clau.de", "claudeusercontent.com",
  "claudemcpclient.com", "claudemcpcontent.com",
  "deepmind.com", "deepmind.google", "ai.google", "net.coffee",
  "chat.com", "chatgpt.site", "crixet.com", "sora.com",
  "gemini.google.com", "bard.google.com", "aistudio.google.com", "makersuite.google.com",
  "flow.google.com", "notebook.google", "labstailwind.pa.googleapis.com", "gemini.google",
  "chat.google.com", "ai.studio", "generativeai.google", "generativelanguage.googleapis.com",
  "ai.google.dev", "aiplatform.googleapis.com", "aicode.googleapis.com", "ai.google.com",
  "gemini.gstatic.com", "labs.google.com", "labs.google", "jules.google",
  "jules.google.com", "antigravity.google", "flow.google", "stitch.withgoogle.com",
  "cloud.google.com", "notebooklm.google.com", "notebooklm.google", "notebook.google.com",
  "perplexity.ai", "mistral.ai", "cohere.ai", "cohere.com",
  "groq.com", "x.ai", "grok.com", "cursor.com",
  "cursor.sh", "cursorapi.com", "cursor-cdn.com", "together.ai",
  "replicate.com", "stability.ai", "midjourney.com", "poe.com",
  "character.ai", "inflection.ai", "pi.ai", "huggingface.co",
  "hf.co", "hf.space", "scite.ai", "muse.ai",
  "ai.meta.com",
];

const staticExact = [
  "challenges.cloudflare.com", "servd-anthropic-website.b-cdn.net",
  "accounts.google.com",
];

function main(config) {
  if (!["http", "socks5"].includes(staticProxy.type) ||
      !Number.isInteger(staticProxy.port) || staticProxy.port < 1 || staticProxy.port > 65535 ||
      [staticProxy.server, staticProxy.username, staticProxy.password].some(value =>
        typeof value !== "string" || !value.trim() || value.startsWith("YOUR_"))) {
    throw new Error("请在脚本顶部填写静态代理的协议、地址、端口、用户名和密码。");
  }

  const list = value => Array.isArray(value) ? value : [];
  const isChain = name => typeof name === "string" && name.startsWith(chainPrefix);
  const proxies = list(config.proxies).filter(proxy => proxy && typeof proxy.name === "string" && !isChain(proxy.name));
  const names = [...new Set(proxies.map(proxy => proxy.name))].filter(name => !infoRe.test(name));
  if (names.length === 0) throw new Error("订阅缺少 proxies 节点。请使用包含独立节点的订阅。");

  // 顺序：固定节点 → 其他美国节点 → 其他地区节点。fallback 取第一个可用的
  const isUS = name => /US|美国/.test(name);
  const rank = name => name.includes(pinnedNode) && pinnedNode ? 0 : isUS(name) ? 1 : 2;
  const ordered = names.map((name, index) => ({ name, index }))
    .sort((a, b) => rank(a.name) - rank(b.name) || a.index - b.index)
    .map(item => item.name);

  const chains = ordered.map(name => ({ ...staticProxy, name: chainPrefix + name, "dialer-proxy": name }));
  const chainNames = chains.map(proxy => proxy.name);
  const groups = list(config["proxy-groups"]).filter(group => group && group.name !== staticGroup);
  if (groups.some(group => chainNames.includes(group.name))) {
    throw new Error("订阅代理组与生成的静态链路同名，请先更改链路前缀或组名。");
  }

  config.proxies = [...proxies, ...chains];
  config["proxy-groups"] = [
    {
      name: staticGroup, type: "fallback", proxies: chainNames,
      url: "https://www.gstatic.com/generate_204", interval: 30, timeout: 5000, lazy: false,
    },
    ...groups,
  ];

  // 静态出口使用 TCP，拒绝 AI 域名的 UDP（如 QUIC），让它回落到 TCP
  const rejectUdp = (kind, domain) => "AND,((NETWORK,UDP),(" + kind + "," + domain + ")),REJECT";
  const stunRules = [
    "AND,((NETWORK,UDP),(DST-PORT,3478)),REJECT",
    "AND,((NETWORK,UDP),(DST-PORT,19302)),REJECT",
  ];
  const aiRules = [
    ...stunRules,
    ...staticDomains.map(domain => rejectUdp("DOMAIN-SUFFIX", domain)),
    ...staticExact.map(domain => rejectUdp("DOMAIN", domain)),
    ...staticDomains.map(domain => "DOMAIN-SUFFIX," + domain + "," + staticGroup),
    ...staticExact.map(domain => "DOMAIN," + domain + "," + staticGroup),
  ];

  // 国内直连插在订阅原规则之后、MATCH 之前，其余订阅规则保持不变
  const original = list(config.rules).filter(rule => typeof rule === "string");
  const cnDirect = ["GEOSITE,CN,DIRECT", "GEOIP,CN,DIRECT,no-resolve"];
  const matchIdx = original.findIndex(rule => /^\s*MATCH\s*,/.test(rule));
  if (matchIdx >= 0) original.splice(matchIdx, 0, ...cnDirect);
  else original.push(...cnDirect);
  config.rules = [...new Set([...aiRules, ...original])];

  // AI 域名用静态组解析，国内域名用国内 DNS，其余 DNS 设置保持订阅原样
  // 默认 DNS 通过订阅的主选择组解析，避免用国内运营商 DNS 解析海外域名
  const dnsGroup = (groups.find(group => group.name === "节点选择") || groups[0])?.name;
  const viaStatic = ["https://1.1.1.1/dns-query#" + staticGroup, "https://8.8.8.8/dns-query#" + staticGroup];
  const viaMain = dnsGroup
    ? ["https://1.1.1.1/dns-query#" + dnsGroup, "https://8.8.8.8/dns-query#" + dnsGroup]
    : config.dns?.nameserver;
  const policy = { ...config.dns?.["nameserver-policy"] };
  staticDomains.forEach(domain => { policy["+." + domain] = viaStatic; });
  staticExact.forEach(domain => { policy[domain] = viaStatic; });
  policy["geosite:cn"] = ["https://223.5.5.5/dns-query"];
  config.dns = {
    ...config.dns,
    enable: true,
    ipv6: false,
    nameserver: viaMain,
    "nameserver-policy": policy,
    "proxy-server-nameserver": ["https://223.5.5.5/dns-query#DIRECT"],
    "default-nameserver": ["223.5.5.5", "119.29.29.29"],
    "prefer-h3": false,
    fallback: [],
  };
  delete config.dns["fallback-filter"];
  return config;
}
