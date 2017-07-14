# authy-haskell

[![][1]][0]

[0]: https://circleci.com/gh/jpvillaisaza/authy-haskell
[1]: https://circleci.com/gh/jpvillaisaza/authy-haskell.svg?style=svg

In terms of libraries, you need to depend on this library, and
depending on your use case, also http-client-tls (if you want to
create your own manager instead of letting authy do it for you), mtl
if you're not running directly to set things up with your config, and
other. As of writing, also servant for servant errors, but this will
be removed.

To use Authy, you need an Authy API key and a manager. Then, you can
use it directly or add it to the configuration of your application.

If using directly,

```haskell
main :: IO ()
main = do
  manager <- getManager False -- or newTlsManager
  let key = "authy-api-key"

  flip runReaderT (key, manager) $
    getPhoneInformation 57 "222222" Nothing
```

(Add a wrapper around runReaderT to make it easier to call it like
this.)

To integrate with your application, you can use the `HasAuthy` class
and add the Authy configuration to the configuration of your
application. If you have a Config:

```haskell
data Config =
  Config
    { authy :: Authy
	}
```

Then make that an instance of HasAuthy:

```haskell
instance HasAuthy Config where
  getAuthy =
    authy
```

And, assuming an app type like follows:

```haskell
type App =
  ReaderT Config IO
```

Then, when running your app, you can use Authy inside:

```haskell
something :: App ()
something = do
  ...
  liftIO . print =<<
    getPhoneInformation 57 "22222" Nothing
  ...
```

Add complete example of 2 factor authentication.

Add complete example of phone verification.

Add complete example of one touch.
