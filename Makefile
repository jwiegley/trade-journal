all: src/Data/Zipper.hs

src/Data/Zipper.hs: src/Data/Zipper.agda
	rm -f agda2hs
	cp $$(which agda) ./agda2hs
	sed -i -e 's%/nix/store.*/bin/agda%agda2hs%' agda2hs
	./agda2hs $< || ./agda2hs $<
	sed -i -e 's/module src\./module /' $@
	sed -i -e 's/survey ::/survey :: forall a./' $@
	sed -i -e 's/surveyM ::/surveyM :: forall m a./' $@
	sed -i -e 's/mapUntils ::/mapUntils :: forall a b./' $@
	ghc -c $@
