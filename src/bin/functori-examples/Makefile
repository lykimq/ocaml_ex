all: ligo michelson factori scenario

ligo:
	@dune build @ligo

michelson: ligo
	@dune build @michelson

factori: ligo michelson
	@dune build @factori

scenario: ligo michelson factori
	@dune build

run_scenario:
	@dune exec src/perfect_number_scenario.exe

clean:
	rm src/factori -rf
	@dune clean
