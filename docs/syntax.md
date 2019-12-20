## Null Checking

```sumo
fn foo(maybeBar: Baz?): int {
  if (var bar = maybeBar) {
    return bar.value
  }
  return -1
}
```