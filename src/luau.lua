local luau = {}

function luau.compile(luau_str: string): any
    local require_success, lune_fs = pcall(function()
        return require("@lune/fs")
    end)
    if require_success and lune_fs then
        -- We use Lune method
        lune_fs.writeFile("./luau_temp_load.luau", `return function() {luau_str} end`)
        return require("../luau_temp_load")
    else
        -- We use Roblox's method
        local mod = Instance.new("ModuleScript")
        mod.Source = `return function() {luau_str} end`
        return require(mod)
    end
end

return luau
