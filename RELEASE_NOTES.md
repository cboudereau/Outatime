## New in 1.2 (Released 2016/4/7)
* refactoring on apply and add lift2
## New in 1.0 (Released 2015/12/10)
* enhanced merge function, avoid toList
## New in 0.1.10 (Released 2015/12/10)
* traverse map : 
	Map<'a, 'b Temporary #seq> -> Map<'a, 'b> Temporary seq
	see : http://fsharpforfunandprofit.com/posts/elevated-world-4/#traverse
	Aggregate accross time intersection, in order to have coherence and applicative functor approach.
	A usefull simple sample can be found into TraverseTests.

## New in 0.1.9 (Released 2015/11/17)
* Applicative functor : function is now an n option type parameter function in order to get a better feedback on missing value on period

## New in 0.1.7 (Released 2015/10/15)
* Applicative functor for value on time interval