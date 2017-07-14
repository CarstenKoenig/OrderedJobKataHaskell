# OrderedJobs Kata

solution to the [Ordered Jobs Kata](http://invalidcast.tumblr.com/post/52980617776/the-ordered-jobs-kata)

demonstrating **Haskell** and [**hspec**](https://hspec.github.io/)

using a simple *topological sort implementation*.

## about the solution to the kata
I tried to follow the given steps (I literally copied the test cases from the
kata's description) and only introduced stuff when I saw fit.

But of course I already had a solution and even my *monad stack* in mind.

Anyway: I hope it at least shows how you could do TDD/BDD in Haskell (if you want)
and a hopefully somewhat nice solution to the Kata.

## running the tests

I recommend using [Stack](https://docs.haskellstack.org/en/stable/README/). With
this it's a simple

    stack test --file-watch
	
to get the TDD started
