require 'global'

vim_map = {
  normal  = {},
  insert  = {},
  command = {},
}

function vim_map:create()
  instance = {}
  setmetatable(instance, self)
  self.__index = self
  return instance
end

function vim_map:define()
  self.normal = {
    ["<C-x>k"] = "BD",
    ["<C-s>"] = "write"
  }
end

function nvim_define_map(items)
  for k,v in pairs(items.normal) do
    vim.api.nvim_set_keymap('n')
  end
end


ins = vim_map:create()
ins:define()
