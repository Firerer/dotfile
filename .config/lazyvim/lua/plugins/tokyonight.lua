return {
  "folke/tokyonight.nvim",
  lazy = true,
  priority = 10000,
  style = "moon",
  opts = {
    transparent = true,
    styles = {
      sidebars = "transparent",
      floats = "transparent",
    },
    -- on_colors = function(colors)
    --   colors.comment = "#ffffff"
    -- end,
    on_highlights = function(hl)
      hl.comment = { bg = "#000000", fg = "#444444" }
      hl.perlComment = { bg = "#000000", fg = "#444444" }
      hl.Comment = { bg = "#000000", fg = "#444444" }
      --hl.IlluminatedWordText = { bg = "#ffffff", fg = "#ffffff" }
    end,
  },
}
