## Authority 
- When you create something, you have the right to use it how you like
- Community members don't necessarily have the same opinions as creators! 

### Three Groups

- Creators
- Consumers
- Contributors

Let's think about the bi-directional relationships between members of these groups.

### Ambient Authority

- The Pony programming language doesn't allow any actor to just do anything!
- You only gain the rights to do certain things like IO through the `env` variable that's passed into your Main actor when the system starts up.
- This allows you to use **Object Capabilities**

```pony
actor Main
    new create( env : Env ) =>
        env.out.print( "Hello, World" )
```

## Links

[Scopes of Authority](Auth.md)

[Correctness](Auth/Correctness.md) / [Liberty](Auth/Liberty.md)
