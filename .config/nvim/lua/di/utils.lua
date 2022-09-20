-- https://github.com/wbthomason/packer.nvim#custom-initialization
_G.packer_bg_update_opt = {
  display = {
    non_interactive = true,
    show_all_info = false,
  },
}

--- Returns a function that can only be run once
---@param fn function
---@return function
_G.make_run_once_function = function(fn)
  local has_run = false
  return function(...)
    if not has_run then
      fn(...)
      has_run = true
    end
  end
end
