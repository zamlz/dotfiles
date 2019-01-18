#! /usr/bin/env ruby

# Copyright © 2016 Quentin "Sardem FF7" Glidic
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

rofi=%w( rofi -dmenu -input /dev/null -password -lines 0)


def assuan_send(t)
    $stdout.puts t
    $stdout.flush
end

assuan_send "OK Please go ahead"
while ( line = $stdin.readline )
    ok = true
    case ( line )
    when /^OPTION (.+)$/
        #OPTION grab
        #OPTION ttyname=/dev/pts/1
        #OPTION ttytype=tmux-256color
        #OPTION lc-messages=C
        #OPTION allow-external-password-cache
        #OPTION default-ok=_OK
        #OPTION default-cancel=_Cancel
        #OPTION default-yes=_Yes
        #OPTION default-no=_No
        #OPTION default-prompt=PIN:
        #OPTION default-pwmngr=_Save in password manager
        #OPTION default-cf-visi=Do you really want to make your passphrase visible on the screen?
        #OPTION default-tt-visi=Make passphrase visible
        #OPTION default-tt-hide=Hide passphrase
        #OPTION touch-file=/run/user/1000/gnupg/S.gpg-agent
    when /^GETINFO (.+)$/
        info = $1
        case ( info )
        when 'pid'
            assuan_send "D #{Process.pid}"
        end
    when /^SETKEYINFO (.+)$/
        #SETKEYINFO s/FINGERPRINT
    when /^SETDESC (.+)$/
        #SETDESC Please enter the passphrase for the ssh key%0A  ke:yf:in:ge:rp:ri:nt
        rofi << '-mesg' << $1.gsub('<', '&lt;').gsub(/%([0-9A-Fa-f]{2})/) { $1.to_i(16).chr }
    when /^SETPROMPT (.+)$/
        #SETPROMPT Passphrase:
        rofi << '-p' << $1
    when /^GETPIN$/
        pass = nil
        #IO.popen(%w( systemctl --user show-environment )) do |io|
        #    until ( io.eof? )
        #        var = io.readline
        #        var.match(/^([A-Za-z_]+)=(.+)$/)
        #        var, val = $~[1..2]
        #        ENV[var] = val
        #    end
        #end
        err_in, err_out = IO.pipe
        IO.popen([*rofi, :err => err_out]) do |io|
            pass = io.read
        end
        status = $?
        err_out.close
        case ( status.exitstatus )
        when 0
            assuan_send "D #{pass}" unless ( pass.empty? )
        when 1
            assuan_send "ERR 83886179 Operation cancelled <rofi>"
            ok = false
        end
    when /^BYE(?= |$)/
        exit(0)
    else
        assuan_send "BYE"
        exit(1)
    end
    assuan_send "OK" if ( ok )
end
