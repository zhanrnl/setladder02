all:
	-pkill sl2http
	-pkill sl2ws
	iced -c static/js/*.iced
	cabal install

js:
	iced -c static/js/*.iced

run:
	sl2http &
	sl2ws &

kill:
	-pkill sl2http
	-pkill sl2ws
