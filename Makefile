all: src/Main.elm
	elm make src/Main.elm --output elm.js
	python3 -m http.server &
