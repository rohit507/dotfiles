for file in ~/.profile.d/*.profile;
do
	chmod +x "$file"  
 	source “$file”
done
