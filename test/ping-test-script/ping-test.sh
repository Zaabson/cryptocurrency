#!/usr/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $DIR

PORT="5040"
SERVER="$DIR/ServerPing.hs"
CLIENT="$DIR/ClientPing.hs"

stack runhaskell $SERVER $PORT &
SERVER_PID=$!

# give server some time before we try to connect 
sleep 0.1

SUCCESS=$(stack runhaskell $CLIENT $PORT)

echo Server answered ping: $SUCCESS

kill $SERVER_PID

# fuser -k $PORT/tcp