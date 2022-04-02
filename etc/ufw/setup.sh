#!/bin/sh

which ufw > /dev/null 2>&1

if [ $? -eq 0 ]; then
    echo "-------------------------------------------"
    echo "Configuring systemd for firewall"
    echo "-------------------------------------------"
    systemctl start ufw.service
    systemctl enable ufw.service
    systemctl status ufw.service

    echo "-------------------------------------------"
    echo "Resetting firewall"
    echo "-------------------------------------------"
    yes | sudo ufw reset

    echo "-------------------------------------------"
    echo "Configuring rules"
    echo "-------------------------------------------"
    ufw logging off
    ufw default deny incoming
    ufw default allow outgoing
    ufw limit ssh

    echo "-------------------------------------------"
    echo "Enabling firewall"
    echo "-------------------------------------------"
    ufw enable
    ufw status verbose
else
    echo "ERROR: 'ufw' is not found!"
fi
