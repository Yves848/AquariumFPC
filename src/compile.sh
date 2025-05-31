systemctl stop aquarium.service
fpc AquariumLightingService.pas -o/usr/local/bin/AquariumLightingService
systemctl start aquarium.service
