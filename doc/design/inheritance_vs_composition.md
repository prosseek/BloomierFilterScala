### [2016/04/10]03:46PM

BF uses BABF as a component instead of inherit from it because BABF can be replaceable.
The load method replaces the BABF by constructing from the bin file.

If BF inherited from BABF, it might be harder, and almost impossible.
The disadvantage is that I need to access the inner parameters from BA accessing parameters, but I don't think this is a big deal.

Likewise, I prefer to component when I need to replace it from time to time.