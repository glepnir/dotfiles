local nvim_lsp = require('nvim_lsp')

function LspConfig()
  -- https://github.com/golang/tools/blob/master/gopls/doc/settings.md#settings
  nvim_lsp.gopls.setup{
    init_options = {
      usePlaceholders=true;
      linkTarget="pkg.go.dev";
      completionDocumentation=true;
      completeUnimported=true;
      deepCompletion=true;
      fuzzyMatching=true;
    };
  }
end
