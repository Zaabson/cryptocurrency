#!/usr/bin/bash/

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $DIR

# stack run -- --config data/config1.json &
# stack run -- --config data/config2.json &
gnome-terminal --tab -- sh -c 'stack run -- --config data/config1.json & sleep 0.2 && tail -f ../node1-logs'
gnome-terminal --tab -- sh -c 'stack run -- --config data/config2.json & sleep 0.2 && tail -f ../node2-logs'

# gnome-terminal --tab -- sh -c 'tail -f ../node1-logs'
# gnome-terminal --tab -- sh -c 'tail -f ../node2-logs'

# stack runhaskell Demo.hs
stack runhaskell Main.hs -- --config data/config3.json