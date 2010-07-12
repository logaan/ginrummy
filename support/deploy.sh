#!/usr/bin/env sh

ssh logaan@ginrummy.logaan.net "cd /var/www/apps/erlang/ginrummy/ && sudo git pull origin master && sudo make"

