#!/bin/sh
# Install nautilus from the typeahead PPA and pin it so that point releases
# in ubuntu -updates (which have higher version numbers) don't replace it.
set -eu

ppa=lubomir-brindza/nautilus-typeahead
origin=LP-PPA-lubomir-brindza-nautilus-typeahead
pin=/etc/apt/preferences.d/nautilus-typeahead

sudo add-apt-repository -y "ppa:$ppa"

sudo tee "$pin" >/dev/null <<EOF
Package: nautilus nautilus-data libnautilus-extension4 libnautilus-extension-dev gir1.2-nautilus-4.1
Pin: release o=$origin
Pin-Priority: 1001
EOF

sudo apt update
sudo apt install -y --allow-downgrades nautilus nautilus-data libnautilus-extension4

nautilus -q || true
apt-cache policy nautilus | head -3
