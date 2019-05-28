for file in ~/.profile.d/*.profile;
do
	chmod +x $file  
 	source $file
done
if [ -e /home/rkr/.nix-profile/etc/profile.d/nix.sh ]; then . /home/rkr/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
