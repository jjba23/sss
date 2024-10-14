JJBA23_DATE:=$(date)

update:
	guix pull
system-reconfigure:
	sudo guix system reconfigure config.scm
sr:
	make system-reconfigure
	make echo-complete
joe-reconfigure:
	guix home reconfigure home/joe/home.scm
	make echo-complete
jr:
	make joe-reconfigure
echo-complete:
	@echo "[guix-jjba23][${JJBA23_DATE}][INFO] Operation was completed successfully!"
full-rebuild:
	make system-reconfigure
	make joe-reconfigure
fr:
	make full-rebuild
gc:
	sudo guix gc --verify=contents,repair
