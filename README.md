# Reconciling Functional Programming and Object Oriented Programming

Some time ago I came across a very interesting post on the Clean-Coder-Blog, 
which kept me busy for months until I finally decided to write this post.

In this post Uncle Bob tries to reconcile FP and OOP concepts by explaining that both approaches are
not mutually exclusive but both provide useful principles that go very well together and in fact are complementary:

> In this blog I will make the case that while OO and FP are orthogonal, 
> they are not mutually exclusive. 
> That a good functional program can (and should) be object oriented. 
> And that a good object oriented program can (and should) be functional. 
>
> [quoted from Uncle Bob's Clean Coder Blog](https://blog.cleancoder.com/uncle-bob/2018/04/13/FPvsOO.html)

He starts his argument by trying to convey the essence of both FP and OOP by giving very condensed definitions. 

## Defining OOP

He gives the following characterisation of OOP:

> The technique of using dynamic polymorphism to call functions without the source code of the caller 
> depending upon the source code of the callee.

With this short stanza Uncle Bob points to the core of object orientation since its first incarnation in
the Smalltalk language:

In an OO language a call of methods on a target object is dispatched based on the target object's type, its `class`.

So a method call `shape.draw()` may invoke different code based on the `class` of the actual shape object.

The code of the `draw` method of class `Rectangle` may be different from the code in `Circle.draw()`.

Client code will just call `shape.draw()`, not even knowing which actual `Shape` sub-class it's working on. This kind of 
polymorphism provides a very useful decoupling of clients from the callees by using the methods of the baseclass `Shape`
as the API for all Objects inheriting `Shape`.

This mechanism allows to build elegant design like the 
[Model-View-Controller (MVC)](https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller) pattern 
which is at the core of Smalltalks GUI and which influenced many similar designs in other OO-languages. 

> MVC is the seminal insight of the whole field of graphical user interfaces. 
> I believe the MVC work was the first to **describe** and implement **software constructs in terms of their responsibilities.** 
> I also believe that MVC was the first significant **use of protocols to define components** instead of using 
> concrete implementations -- each controller class had a certain set of messages it had to respond to, 
> as did each view class, but otherwise there were no constraints on what they did and how they did it.
>
> [quoted from the C2 Wiki](http://wiki.c2.com/?ModelViewControllerHistory)

This quote conveys two major achievements of OOP:

1. Decomposing software into separate components with distinct responsibilities
2. Using protocols - APIs or interfaces in todays lingo - to decouple those components and allow for 
varying implementations.

## Defining FP

Uncle Bob then gives a very brief characterization of functional programming:

> Referential Transparency – no reassignment of values.
>
> [quoted from Uncle Bob's Clean Coder Blog](https://blog.cleancoder.com/uncle-bob/2018/04/13/FPvsOO.html)

Referential transparency is implying **purity** as explained in the following definition from Wikipedia:

> An expression is called **referentially transparent if it can be replaced with its corresponding value**
> (and vice-versa) without changing the program's behavior. 
> This **requires that the expression be pure**, that is to say **the expression value must be the same for the 
> same inputs** and its **evaluation must have no side effects**.
> 
> [quoted from Wikipedia](https://en.wikipedia.org/wiki/Referential_transparency)

The second part of Uncle Bob's stanza may be implied by this definition, but I prefer to see it as separate 
yet closely related principle, namely **immutability**:

> In object-oriented and functional programming,  an immutable object (unchangeable object) is an object whose **state 
> cannot be modified after it is created**. [...]
> 
> [Quoted from Wikipedia](https://en.wikipedia.org/wiki/Immutable_object) 
 
## There is no FP vs OOP

After this dense characterization of the two programming paradigms Uncle Bob continues his arguments like follows:

The concepts of Polymorphism and Referential Transparency are orthogonal. You can have Polymorphism without
Referential Transparency – and vice versa.

But orthogonality does not imply that both concepts are mutually exclusive. 
It is possible to have languages that support both Dynamic Polymorphism and Referential Transparency.  
It is not only possible, but even desirable to combine both concepts. 

1. Dynamic Polymorphism is desirable as it allows building strongly decoupled designs.

    > Dependencies can be inverted across architectural boundaries. 
    > They are testable using Mocks and Fakes and other kinds of Test Doubles. 
    > Modules can be modified without forcing changes to other modules. 
    > This makes such systems much easier to change and improve.
    > 
    > Uncle Bob

2. Referential Transparency is desirable as it allows designs that are much easier to understand, to reason about,
   to change and to improve. It also allows designs that are much better suited for scalability and concurrency
   as the chances of race conditions etc. are drastically reduced.  
  
Uncle Bob concludes that Dynamic Polymorphism and Referential Transparency are both desirable as part 
of software systems: 

> A system that is built on both OO and FP principles will **maximize flexibility, 
> maintainability, testability, simplicity, and robustness**.   
> 
> Uncle Bob
  
## Ad-hoc Polymorphism and Referential Transparency in Haskell

In this section I'm showcasing how those two concepts are supported in Haskell and can be combined without sacrificing FP principles.

1. **Referential Transparency**
    
    Haskell is one of the rare incarnations of a purely functional language.
    So it goes without saying that Referential Transparency, Purity and Immutability are a given in Haskell.
    Yes, there are things like `unsafePerformIO` but overall
    it's very easy to write clean code in Haskell due to the strict separation of pure and impure code.
    
    Referential Transparency in Haskell is so much a given that it's quite possible to apply
    equational reasoning to proof certain properties of Haskell programs.
    See for example the following [Proof of Functor laws for the Maybe type](https://github.com/thma/WhyHaskellMatters/blob/master/functor-proof.md).
    What's remarkable here is that you can use the same language to write your code and to 
    reason about it. 
    This is not possible in languages that do not provide Referential Transparency. To reason about
    programs in such languages you'll have to use external models like an abstract stack + register machine.
    

2. **Ad-hoc Polymorphism** 

    Being able to overload functions and operators with different implementations depending on the 
    type of its arguments is called Ad-hoc Polymorphism.
    For example, the `+` operator does something entirely different when applied to floating-point 
    values as compared to when applied to integers. In Haskell, this kind of polymorphism is achieved with 
    type classes and class instances.
    
    Haskell's type classes are quite different from the classes 
    in OOP languages. Yet they have more in common with interfaces, in that they 
    specify a set of functions with their respective type signatures, to be implemented by instance 
    declarations.

Let's have a look at a simple example.



**Still work in progress**

 

