# OrderedJobs Kata

solution to the [Ordered Jobs Kata](http://invalidcast.tumblr.com/post/52980617776/the-ordered-jobs-kata)

demonstrating **Haskell** and [**hspec**](https://hspec.github.io/)

using a simple *topological sort implementation*.

## about the solution to the kata
I tried to follow the given steps (I literally copied the test cases from the
kata's description) and only introduced stuff when I saw fit.

If you want to find out the steps it's probably best to look at the commits
and see the diffs - there is usually only one commit for each test case.

A big **thumb up** to those test cases - they actually guide you to one of
the popular [topological sort algorithms](https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search) 
with ease.

Anyway: I hope this demonstrates how you could do TDD/BDD in Haskell (if you 
want) and a hopefully somewhat nice solution to the Kata.

## running the tests

I recommend using [Stack](https://docs.haskellstack.org/en/stable/README/). With
this it's a simple

    stack test --file-watch
	
to get the TDD started
