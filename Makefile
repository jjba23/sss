# Makefile - installing and working with sss - Supreme Sexp System

# Copyright (C) 2024 Josep Bigorra

# Version: 0.1.0
# Author: Josep Bigorra <jjbigorra@gmail.com>
# Maintainer: Josep Bigorra <jjbigorra@gmail.com>
# URL: https://github.com/jjba23/sss

# sss is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# sss is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with sss.  If not, see <https://www.gnu.org/licenses/>.

# Commentary:

# TODO

# Code:


define sss-log-complete
	@printf "\n>>= $(shell date) - success - target %s was completed!\n" $(1)
endef

define sss-log-info
	@printf "\n>>= $(shell date) - info - %s \n" $(1)
endef

update:
	@guix pull
system-reconfigure:
	$(call sss-log-info,"begin working on system-reconfigure target")
	@sudo guix system reconfigure config.scm
	$(call sss-log-complete,"system-reconfigure")
joe-reconfigure:
	$(call sss-log-info,"begin working on joe-reconfigure target")
	@guix home reconfigure home/joe/home.scm
	$(call sss-log-complete,"joe-reconfigure")
manon-reconfigure:
	$(call sss-log-info,"begin working on manon-reconfigure target")
	@guix home reconfigure home/manon/home.scm
	$(call sss-log-complete,"manon-reconfigure")
full-rebuild:
	@make system-reconfigure
	@make joe-reconfigure
	@sudo fc-cache -r
	@fc-cache -r
	$(call sss-log-complete,"full-rebuild")

# Aliases section
jr:
	@make joe-reconfigure
mr:
	@make manon-reconfigure
sr:
	@make system-reconfigure
fr:
	@make full-rebuild
gc:
	@sudo guix gc --verify=contents,repair
