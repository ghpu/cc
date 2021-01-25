all: src/Main.elm
	elm make --optimize src/Main.elm --output elm.js
	python3 -m http.server &
