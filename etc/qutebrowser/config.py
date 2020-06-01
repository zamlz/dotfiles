# My custom config.py

# Obtain the current colors I use
def read_colorscheme(path):
    with open(path, 'r') as f:
        return dict(l[8:].split() for l in f.readlines() if l[:7] == '#define')

cs = read_colorscheme('/home/zamlz/.config/xcolor/scheme')

## Always restore open sites when qutebrowser is reopened.
## Type: Bool
c.auto_save.session = True

## Background color of the completion widget category headers.
## Type: QssColor
# c.colors.completion.category.bg = 'qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 #888888, stop:1 #505050)'
c.colors.completion.category.bg = cs['xcolor0']

## Foreground color of completion widget category headers.
## Type: QtColor
c.colors.completion.category.fg = cs['xforeground']

## Background color of the completion widget for even rows.
## Type: QssColor
c.colors.completion.even.bg = cs['xcolor0']

## Text color of the completion widget. May be a single color to use for
## all columns or a list of three colors, one for each column.
## Type: List of QtColor, or QtColor
c.colors.completion.fg = [cs['xforeground']]*3

## Background color of the selected completion item.
## Type: QssColor
c.colors.completion.item.selected.bg = '#00000000' # cs['xbackground']

## Bottom border color of the selected completion item.
## Type: QssColor
c.colors.completion.item.selected.border.bottom = cs['xcolor4']

## Top border color of the completion widget category headers.
## Type: QssColor
c.colors.completion.item.selected.border.top = cs['xcolor4']

## Foreground color of the selected completion item.
## Type: QtColor
c.colors.completion.item.selected.fg = cs['xcolor15']

## Foreground color of the matched text in the completion.
## Type: QssColor
c.colors.completion.match.fg = cs['xcolor4']

## Background color of the completion widget for odd rows.
## Type: QssColor
c.colors.completion.odd.bg = cs['xbackground']

## Background color of unselected even tabs.
## Type: QtColor
c.colors.tabs.even.bg = cs['xbackground']

## Foreground color of unselected even tabs.
## Type: QtColor
c.colors.tabs.even.fg = cs['xcolor8']

## Background color of unselected odd tabs.
## Type: QtColor
c.colors.tabs.odd.bg = cs['xbackground']

## Foreground color of unselected odd tabs.
## Type: QtColor
c.colors.tabs.odd.fg = cs['xcolor8']

## Background color of selected even tabs.
## Type: QtColor
c.colors.tabs.selected.even.bg = "#383838" # cs['xcolor0']

## Foreground color of selected even tabs.
## Type: QtColor
c.colors.tabs.selected.even.fg = cs['xforeground']

## Background color of selected odd tabs.
## Type: QtColor
c.colors.tabs.selected.odd.bg = "#383838" # cs['xcolor0']

## Foreground color of selected odd tabs.
## Type: QtColor
c.colors.tabs.selected.odd.fg = cs['xforeground']

## Directory to save downloads to. If unset, a sensible OS-specific
## default is used.
## Type: Directory
c.downloads.location.directory = "~/tmp"

## Default monospace fonts. Whenever "monospace" is used in a font
## setting, it's replaced with the fonts listed here.
## Type: Font
c.fonts.default_family = ['Dina', "xos4 Terminus", 'Terminus, Monospace',
        "DejaVu Sans Mono", 'Monaco', "Bitstream Vera Sans Mono",
        "Andale Mono", "Courier New", 'Courier', "Liberation Mono",
        'monospace', 'Fixed', 'Consolas', 'Terminal']
