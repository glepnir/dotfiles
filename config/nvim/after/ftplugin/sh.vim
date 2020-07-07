call coc#config('languageserver', {
    \ 'bash': {
      \ "command": "bash-language-server",
      \ "args": ["start"],
      \ "filetypes": ["sh"],
      \ "ignoredRootPaths": ["~"]
      \ }
      \})
