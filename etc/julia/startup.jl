using OhMyREPL
using Crayons

import OhMyREPL: Passes.SyntaxHighlighter

scheme = SyntaxHighlighter.ColorScheme()

# Get colorscheme from file
xcolors = Dict()
open(ENV["HOME"] * "/.config/xcolor/scheme", "r") do file
    for line in eachline(file)
        spl = split(line)
        if length(spl) != 0
            if spl[1] == "#define"

                xsym = Symbol(spl[2])
                xval = replace(spl[3], "#" => "")
                xval = parse(UInt32, xval, base=16)

                xcolors[xsym] = xval
            end
        end
    end
end

SyntaxHighlighter.symbol!(scheme, Crayon(foreground=xcolors[:xcolor3], bold=true))
SyntaxHighlighter.comment!(scheme, Crayon(foreground=xcolors[:xcolor8]))
SyntaxHighlighter.string!(scheme, Crayon(foreground=xcolors[:xcolor2]))
SyntaxHighlighter.call!(scheme, Crayon(foreground=xcolors[:xcolor4]))
SyntaxHighlighter.op!(scheme, Crayon(foreground=xcolors[:xcolor5]))
SyntaxHighlighter.keyword!(scheme, Crayon(foreground=xcolors[:xcolor3]))
SyntaxHighlighter.macro!(scheme, Crayon(foreground=xcolors[:xcolor1], bold=true))
SyntaxHighlighter.function_def!(scheme, Crayon(foreground=xcolors[:xcolor4], bold=true))
SyntaxHighlighter.text!(scheme, Crayon(foreground=xcolors[:xforeground]))
SyntaxHighlighter.error!(scheme, Crayon(foreground=xcolors[:xcolor1]))
SyntaxHighlighter.argdef!(scheme, Crayon(foreground=xcolors[:xcolor5], bold=true))
SyntaxHighlighter.number!(scheme, Crayon(foreground=xcolors[:xcolor6]))

SyntaxHighlighter.add!("zamlz", scheme)
colorscheme!("zamlz")
