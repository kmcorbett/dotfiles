#!/bin/bash

# tunnel2code - get TCP/IP access over VPN to Verisk code server host CODE.VERISK.COM
# Run this as root on Mac OS X or Linux to set up IP port tunnels over ssh
# Changes local hosts entry for CODE.VERISK.COM to redirect to VPN ssh server host
# Sets up ssh tunnels on ports 443 and 7999 to redirect to ssh server host
# Authenticates to remote ssh using RSA private key in $HOME/.ssh/id_rsa if found 
# Kill the script (^C) to tear down ssh tunnels and restore hosts table.

set -u

# All script command args are passed to ssh
ARGS=$*

# Code host is the server we want to reach for HTTPS and git/Stash
CODEHOST_IP=10.16.91.42

# Tunnel ports 443 (HTTPS) and 7999 (git/Stash)
LOCAL_HTTPS_PORT=443
REMOTE_HTTPS_PORT=$LOCAL_HTTPS_PORT
LOCAL_STASH_PORT=7999
REMOTE_STASH_PORT=$LOCAL_STASH_PORT

# VPN ssh server must be accessible locally (presumably over VPN) and
#  must have access to the remote code host.
#VPNSSHSERVER_IP=10.48.20.12
#VPNSSHSERVER_USER=kcorbett
VPNSSHSERVER_IP=192.168.67.119
VPNSSHSERVER_USER=crucible

# Use RSA private key if found
# Deliberately uses $HOME to find key for effective user before 'sudo'
IDENT_FILE=$HOME/.ssh/id_rsa
[ -r $IDENT_FILE ] && ARGS="-i $IDENT_FILE $ARGS"

# If this were a real program we would declare these variables
TUNNEL1=
TUNNEL2=
LOCAL_REDIRECT_IP=
IFCONFIG_UP=
IFCONFIG_DOWN=

if [ ! -e "/private/etc/hosts" ] ; then
    echo "On Linux"
    LOCAL_REDIRECT_IP=$VPNSSHSERVER_IP
    TUNNEL1="$LOCAL_HTTPS_PORT:$CODEHOST_IP:$REMOTE_HTTPS_PORT"
    TUNNEL2="$LOCAL_STASH_PORT:$CODEHOST_IP:$REMOTE_STASH_PORT"
    IFCONFIG_UP="ifconfig lo:1 $LOCAL_IP"
    IFCONFIG_DOWN="ifconfig lo:1 down"
else
    echo "On a Mac"
    LOCAL_HTTPS_PORT=8081
    LOCAL_IP1=172.16.123.1
    LOCAL_IP2=172.16.123.2
    LOCAL_REDIRECT_IP=$LOCAL_IP2
    TUNNEL1="$LOCAL_IP1:$LOCAL_HTTPS_PORT:$CODEHOST_IP:$REMOTE_HTTPS_PORT"
    TUNNEL2="$LOCAL_IP1:$LOCAL_STASH_PORT:$CODEHOST_IP:$REMOTE_STASH_PORT"
    IFCONFIG_UP=
    IFCONFIG_DOWN=
    #
    ifconfig lo0 alias "$LOCAL_IP1"
    ifconfig lo0 alias "$LOCAL_IP2"
    ipfw add 100 fwd "$LOCAL_IP1","$LOCAL_HTTPS_PORT" tcp from any to "$LOCAL_IP2" "$REMOTE_HTTPS_PORT"
    ipfw add 100 fwd "$LOCAL_IP1","$LOCAL_STASH_PORT" tcp from any to "$LOCAL_IP2" "$REMOTE_STASH_PORT"
fi

# Cleanup function
function cleanup {
    echo "Cleaning up..."
    $IFCONFIG_DOWN
    cp /etc/hosts.bak /etc/hosts
    echo "Done."
    exit
}

# Overwrite host IP address for remote code host
cp /etc/hosts /etc/hosts.bak
( echo "$LOCAL_REDIRECT_IP code.verisk.com" ; grep -v code.verisk.com ) \
    < /etc/hosts.bak  > /etc/hosts

# Start tunnels, catch ^C to terminate
trap cleanup SIGHUP SIGINT SIGTERM
echo "Tunneling..."

$IFCONFIG_UP
ARGS="$ARGS -L $TUNNEL1 -L $TUNNEL2 -l $VPNSSHSERVER_USER -N $VPNSSHSERVER_IP"
ssh $ARGS

echo "Interrupted."
cleanup
