===
P4P
===

p4p ("peer-for-peer"), a p2p networking library, in Haskell.

This is currently **EXPERIMENTAL**.

This is not your average networking library, it is *exceedingly opinionated* on
how programs should be structured. The main theme is to architect protocol
implementations against a simple interface to an abstract runtime environment,
rather than against the complex ones provided by popular operating systems.
This makes it it easier to match a specification with its implementation, and
enables advanced features not easily achievable in typical programs, described
below. It also includes various support frameworks that help to reduce the cost
of adhering to these structure and architecture prescriptions, that recover the
convenience of the more complex runtime interfaces.

.. contents::


Features
========

Implemented so far:

- Completely deterministic execution.

- Suspend execution at arbitrary points into a serialisable state, which can
  be arbitrarily copied and sent to others, e.g. for testing or analysis.

- Resume or fork previously-suspended execution state.

- Record inputs and outputs, replay an execution on the same inputs and
  verify that the outputs are the same i.e. that the implementation indeed is
  deterministic, run a tweaked execution on slightly different inputs and
  see how this changes the output and behaviour.

- All of the above is achieved *without* invasive low-level tooling, and can
  be done on live production instances. That is, the approach automatically
  enables advanced instrumentation features, without explicit code for it.

- All of the above also applies to a whole-network simulation, as well as the
  execution of individual processes.

Planned:

- Actual examples of implemented protocols that are reasonably complex.

- See TODO file, and grep for various TODO in the source code.

Use-cases
---------

One major use-case is as follows:

Typically, debugging production software involves lots of logging. However, no
existing general logging system supports replaying the logs against the code,
so that you can easily empirically test your theories as you attempt to figure
out why the code exhibited a certain bug, or to write a fix for it. Instead,
you must recreate the conditions of the bug afresh, which relies on you
understanding it in the first place. This cyclic dependency can mean that the
start of an attempt to debug a problem in a complex protocol can be very slow,
especially in decentralised production environments without access to all
nodes. Being able to replay the logs against both buggy and potentially-fixed
versions of the code helps to greatly speed up this process.


Background
===========

What do we mean by determinism? Actually, technically all computer programs are
already deterministic in principle - given all of the inputs to an execution of
the program, there is only really one way to execute it, since the instructions
are unambiguous. However, recording all of the inputs is next-to-impossible to
achieve in practice, since this includes things such as:

- when different threads read from / wrote to shared resources relative to each
  other, which includes:

  - shared memory
  - file descriptors / sockets

- when different threads were suspended and resumed relative to each other,
  which is affected by:

  - when timers were fired
  - any other interrupts such as hardware interrupts
  - the scheduler, which itself is affected by potentially-all other processes
    running on the machine, or resource-usage-separation context

- possibly other things I've overlooked here

In theory, if the kernel records all of this input information, it should be
able to replay an execution of a program deterministically and reproduce the
recorded behaviour completely faithfully. So for example, a more sophisticated
version of DTrace might in theory be able to do this, for all programs.

However, even if it's theoretically possible, there are still several downsides
to this approach:

- it is very hard to achieve, and has to be maintained and extended as new
  services (e.g. new hardware support) are added to the operating system.

- there is an insane amount of input you have to record, and the uninteresting
  parts (i.e. thread suspensions unrelated to the application) overwhelm the
  interesting parts, making the actual application logic hard to analyse.

- replaying all of this input will be slow, since it involves recreating
  low-level details accurately, which intuitively would probably involve more
  overhead than running them in their "natural" original way.

Principles
==========

In this library we take a different approach. Instead of using a very rich
runtime environment, we purposefully only provide a greatly simplified one.
Programs must be defined as pure state machines, that can only interact with
this runtime via a stream of inputs and outputs. In addition, the inputs,
outputs, and execution state must all be serialisable.

These constraints give us the ability to execute programs deterministically,
and to do advanced high-level things with their execution, such as those listed
in `Features`_.

A note on terminology: *program* refers to a static piece of code that defines
a behaviour; whereas *process* refers to a running instance of a program,
including its runtime state, which changes over the lifetime of execution, and
can be very different across different executions of the same program.

To expand in detail upon the high-level summary above: the runtime environment
consists of the following services, designed as pure abstractions without
depending on operating system services (as the Haskell IO monad does):

- timers - ``Data.Schedule``
- RNGs - ``Crypto.Random.Extra``
- network and user I/O - ``P4P.Proc``

Our focus on easy serialisation comes at a cost, namely being unable to use
closures/continuations when implementing callbacks such as timers and
futures/promises. However, we observe that in nearly all secure programs, we do
not need to run *arbitrary* callbacks to handle these sorts of events, rather
they are always selected from some predetermined set of behaviours, and the
input parameters to these behaviours are always serialisable. This is not a new
technique or concept - it's called "defunctionalization" and has been around
for several decades. We provide some higher-level utilities to make this
technique easier to use for each new protocol that needs it.

Limitations
-----------

Our design is focused around network protocols and should work nicely for
communication programs that are generally I/O-bound. Nevertheless, these
programs sometimes also have heavy computation or storage needs. Our strategy
for dealing with this is described further below in `Challenges`_.

For programs that are CPU-bound, our model is not the most suitable. However,
these programs generally have predictable I/O sources that are static across
the lifetime of the execution - e.g. think scientific, analysis, or production
(e.g. compiler) tools. That is, they don't make much use of the complex runtime
environment provided by the operating system. Hence, it is already easy to
implement them in a deterministic way, and to perform all the advanced features
we described without needing any special support frameworks such as this one.

For very complex programs such as browsers, IDEs, and some games, they can be
split into I/O-bound parent programs that delegate heavy computations to
CPU-bound child programs, thus adopting a hybrid of the above approaches.

Programming language
--------------------

Since one big focus of this library is to design simple abstractions for
runtime environments, we chose Haskell. Syntactically, it is very clean at
expressing abstractions, whereas with Rust and OCaml for slightly more advanced
abstractions, one has to write much more syntactic boilerplate and punctuation
than the equivalent things in Haskell.

We don't make much use of evaluation laziness (a controversial distinguishing
feature of Haskell) and our data structures are mostly explicitly defined to be
strict, like other languages do by default. Therefore, it should be easy to
translate this library into either Rust or OCaml when ready if desired.

Haskell is also much more performant that people give it credit for - the GHC
optimiser is pretty good and Haskell comes high in the `Debian language
benchmarks`_, coming solidly in "tier 2" next to Swift, Go and OCaml, "tier 1"
being of course C/C++/Rust.

For the few specific performance-critical things, such as certain cryptographic
primitives, these are implemented in C/C++ by the relevant Haskell library.
Otherwise, as mentioned earlier p2p programs generally are I/O bound, and
Haskell does sufficiently well in such situations.

There is also work on the horizon (i.e. in several years' time) that would make
the language even faster - such as Linear Haskell which eventually will be able
to optimise away unnecessary state copies whilst retaining the ability to clone
the whole state at will.

.. _Debian language benchmarks: https://benchmarksgame-team.pages.debian.net/benchmarksgame/which-programs-are-fastest.html

Challenges
==========

Our approach is currently experimental and there are only a few other projects
in the world adopting similar principles (e.g. some unikernels). There are
several challenges to be overcome before we can deliver on its vision.

One key challenge being, can we actually implement real-world protocols using
this simplified runtime environment? Perhaps the constraints placed on the
developer are too costly for real-world usage? This is certainly a risk,
similar to how Haskell constrains the programmer and as a result has never been
one of the most popular programming languages. However, one lesson to be learnt
from Haskell (and elsewhere) is that placing heavy constraints upon yourself,
pushes you to explore concepts to their ultimate depth, discovering more
thorough and universal abstractions useful for future creations. We believe
that these benefits are worth pursuing as a technology to exist in the world,
even if it's not the most popular in terms of usage.

Further R&D work can also mitigate this factor - to carry on the analogy with
Haskell, modern Haskell today in 2020 is certainly easier to use than Haskell
from the 1990s. Similarly, by gaining experience in writing more real-world
protocols, we develop more support frameworks that will reduce the effort
involved in writing new future protocols. Notably, we already have previous
experience in doing this, and have designed our simplified runtime environment
around the core essential needs of typical protocols, omitting typical
operating system services that we've found to be unessential.

As mentioned in `Limitations`_, sometimes even communication programs have
heavy computation or storage requirements. We have yet to explore the design
space here thoroughly to figure out the best model for supporting these.

One main problem is that both computation and storage can block the thread of
execution, preventing other I/O from being handled in the meantime. One option
that addresses this, is to split these parts of the program into separate child
programs that performs the heavy computation or provides piecemeal access to
the storage, and communicates with the parent program via a new set of inputs
and outputs just like how other external processes communicate. This is fairly
realistic, and forces the parent program to handle errors such as disk failures
or computations running out of memory. Whether we can do practical programming
under this model is yet to be explored.

Deterministic execution of a computation-heavy child program is easy; that of a
storage-heavy child program is hard. However, as long as the communication
between the child and parent programs are recorded, deterministic execution of
the top-level parent program can still be achieved.

Some more specific things are mentioned in the TODO file.

A long-term challenge is to keep the simple runtime environment simple.
Operating systems did not start off being as complex as today's - older systems
were much simpler. But over time, more and more features were incrementally
added to the runtime system interface, and exposed to all programs even though
most of them did not need them. New programs were tempted to use these new
features simply because they were available, even if they made the program more
complex than what was needed to accomplish their task.

We'll have to figure out how best to avoid repeating this mistake. Our focus on
abstraction certainly helps - programs are encouraged to be written against
abstract I/O interfaces, instead of specific networking sockets or filesystems.
