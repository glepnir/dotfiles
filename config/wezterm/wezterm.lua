local wt = require("wezterm")

return {
	enable_tab_bar = false,
	font = wt.font("Operator Mono SSm Lig", { weight = 325 }),
	font_size = 16,
	color_scheme = "Andromeda",
	custom_block_glyphs = true,
	window_decorations = "RESIZE",
	window_close_confirmation = "NeverPrompt",
}
