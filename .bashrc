for file in ~/.bashrc.d/*.bashrc;
do 
	chmod +x "$file"  
	source “$file”
done 
