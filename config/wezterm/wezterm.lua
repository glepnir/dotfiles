local wt = require('wezterm')

return {
  enable_tab_bar = false,
  font = wt.font('Operator Mono Lig',{weight = 325}),
  font_size = 17,
  color_scheme = "Andromeda",
  custom_block_glyphs = true,
  window_decorations = "RESIZE",
  window_close_confirmation = "NeverPrompt",
}
