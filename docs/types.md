## Primitives
A few types are built directly into the Sumo language, and ultimately
provide the basis for all other types:
* `int` - a 64-bit signed integer
* `double` - a 64-bit double-precision number
* `bool` - either `true` or `false`
* `T[]` - a contiguous memory region containing a number
of `T` objects. Supports the `[index]` operator, and also
has a single `length` property. Arrays are passed by value.

## Optionals
Sumo has no concept of a pointer, and instead has *optional types*.
Non-primitive types are passed by reference, so in terms of emitted code,
there is no difference; however, a null-check is required to access their values
in Sumo code.

A primitive type `T`, when wrapped in the optional type `T?`, will be passed as a
structure taking up `sizeof(T) + 1` bytes of space. In C++, it might look like this:

```c++
template <typename T>
struct SumoOptional {
  bool exists;
  T value;
};
```

## Records
Records are user-defined **pass-by-value** types. Records
must have a name starting with a lowercase letter, and
cannot have any methods. This makes them fine for use to
aggregate static data, but it is important to remember that
they are never passed by reference.

Records have the operators `==`, `!=`, `<`, `<=`, `>`, and
`>=` defined automatically.

```sumo
record two_ints {
  a: int
  b: int
}
```

## Classes
Classes are user-defined **pass-by-reference** types.
The Sumo runtime handles memory management of class
instances via garbage collection. Classes, like records,
can be used to aggregate data. Class fields, however,
can have `public`, `protected`, or `private` visibility.
In addition, classes can have *methods*, which are special
functions that have access to all of an instance's data.
Classes do not have any operators defined automatically,
except for `==` and `!=`, which merely check for pointer
equality.

The distinction between records and classes is important,
because classes hold additional data at runtime, besides
just their fields. Most notably, they also hold a
*virtual method table*, which is an array of pointers
to functions, which correspond to class methods.

Class names must begin with an uppercase letter.

Class fields and methods are, by default, `private`.

A class can also have one `constructor`, which is invoked
when a new instance of the class is allocated. Special
"this parameters," like `this.x`, will be used to initialize
class members.

```sumo
class TwoInts {
  a: int
  b: int

  constructor(this.a, this.b)

  pub fn sum() : int -> a + b
}

fn main() : int {
  var instance = TwoInts(2, 4)
  instance.sum() // 6
}
```

Classes can be `abstract`. Abstract class methods may
optionally have no body at all. An abstract class may
not be instantiated directly, but it may provide common
functionality.

A class can extend **one** other class. Note that
the `override` keyword is necessary. Although it is
more verbose, it aids readability.

```sumo
abstract class IntThunk {
  pub fn compute(): int
}

class TwoThunk : IntThunk {
  override pub fn compute() -> 2
}
```