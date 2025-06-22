#!/usr/bin/zsh
# Fichiers source
CONFIG_JSON="./data/config.json"
SOURCE_PAS="./src/AquariumLightingService.pas"
SOURCE_HTML="./html"
# Destinations
DEST_CONFIG="/etc/aquarium/"
DEST_SOURCE="/root/sources/"
DEST_HTML="/root/sources/"
# Copier les fichiers
echo "📁 Copie de $CONFIG_JSON vers $DEST_CONFIG"
scp "$CONFIG_JSON" root@192.168.50.205:"$DEST_CONFIG" || { echo "❌ Échec config.json"; exit 1 }

echo "📁 Copie de $SOURCE_PAS vers $DEST_SOURCE"
scp "$SOURCE_PAS" root@192.168.50.205:"$DEST_SOURCE" || { echo "❌ Échec .pas"; exit 1 }

echo "📁 Copie de $SOURCE_HTML vers $DEST_HTML"
scp -r "$SOURCE_HTML" root@192.168.50.205:"$DEST_HTML" || { echo "❌ Échec .pas"; exit 1 }

echo "✅ Tous les fichiers ont été copiés avec succès !"
