# Compose Compiler / Editor Mark Zero Triple Prime

This is a bootstrap compiler and editor for Compose: `TSCE0`, written in [TypeScript].

## Building / Running

I use [Yarn] to do package management and invoke the build scripts, but you should be able to use
[NPM] if you prefer it. Hopefully if you are familiar enough with these tools to have a preference,
you can manage to translate the instructions appropriately.

Do the standard chicken sacrifice to download hundreds of thousands of lines of dubious,
untrustworthy dependencies:

```
yarn install
```

Then you can build and run the test server which handles recompiling on file changes and serving up
the test editor like so:

```
yarn run start
```

Finally navigate to `http://localhost:3000/test/` and you should see the test compiler/editor in
whatever glorious state of disarray it presently occupies.

It should hopefully not be completely broken, but it is unlikely to be a particularly user friendly
experience. So prod at it without remorse, then shake your head a bit in dismay and go back to
whatever you were doing.

[NPM]: https://www.npmjs.com/
[Yarn]: https://yarnpkg.com/en/
[TypeScript]: http://www.typescriptlang.org/
