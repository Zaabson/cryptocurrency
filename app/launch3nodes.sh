#!/usr/bin/bash/

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $DIR

stack runhaskell Main.hs -- --config data/config1.json &
stack runhaskell Main.hs -- --config data/config2.json &
# stack runhaskell ../app/Main.hs -- --config ../app/config3.json
stack runhaskell Demo.hs