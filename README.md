# Convert FE2's EventScript to the new Timelines system
This is an attempt at making a better conversation of EventScript to XFrames.
Instead of using string.gsub, this use a custom Luau parser that can parse code into Abstract Syntax Tree (AST), which can be used to analyze the scipt to determine what it's doing, and then create the XFrames.
Original plugin by Daitsuki: https://www.roblox.com/library/14197514618/FE2-EventScript-to-XFrame-Converter

## Usage
not yet

## Credits
- [@Daitsuki](https://about.me/daitsukidaaaaa): The original plugin
- [optimisticside/luaul](https://github.com/optimisticside/luaul) - The impressive Luau parser
- [TechHog8984/LuauAstLua](https://github.com/TechHog8984/LuauAstLua) - Used code as reference to reimplement missing features for luaul's parser
- Lelerism (yay fren) - New luau parser
- LacticMilk - Assisted me with the Converter module