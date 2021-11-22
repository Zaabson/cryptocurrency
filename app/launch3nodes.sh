#!/usr/bin/bash/

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $DIR

stack run -- --config data/config1.json &
stack run -- --config data/config2.json &
stack run -- --config data/config3.json
# stack runhaskell Demo.hs
# stack runhaskell Main.hs -- --config data/config3.json