local function fmt_nix(file)
  vim.fn.jobstart({ "nixpkgs-fmt", file }, {
    on_exit = function(_, code)
      if code == 0 then
        print "nixpkgs-fmt success"
        vim.cmd.edit()
      else
        print "nixpkgs-fmt failed"
      end
    end,
  })
end

vim.keymap.set(
  "n",
-- cannot overwrite global mapping <leader>lf
  "<leader>cf",
  function() fmt_nix(vim.fn.expand "%") end,
  { noremap = true, desc = "nixpkgs-fmt", silent = true, buffer = 0 }
)

vim.opt_local.commentstring = "# %s"
