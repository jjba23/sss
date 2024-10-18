SSS_DATE:=$(date)

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
manon-reconfigure:
	guix home reconfigure home/manon/home.scm
	make echo-complete
jr:
	make joe-reconfigure
mr:
	make manon-reconfigure
echo-complete:
	@echo "[sss][${SSS_DATE}][INFO] Operation was completed successfully!"
full-rebuild:
	make system-reconfigure
	make joe-reconfigure
	sudo fc-cache -rv
	fc-cache -rv
fr:
	make full-rebuild
gc:
	sudo guix gc --verify=contents,repair
