1. Run contract in README.md

2. Check what contracts are present:

```
curl -s http://localhost:8080/api/new/contract/definitions | jq
```

You should receive a list of contracts and the endpoints that can be called on them, and the arguments
required for those endpoints.

We're interested in the `GameContract` one.

#### Playing the guessing game over the API

The game has two players (wallets). One will initialise the contract and lock a value inside. Another
wallet will then make guesses. Supposing they guess correctly, they'll receive the funds that were
locked; otherwise, they won't!

1. Start the instances:

```
# Wallet 1
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "CurrencyContract", "caWallet":{"getWallet": 1}}' \
  http://localhost:8080/api/new/contract/activate | jq

# Wallet 2
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "CurrencyContract", "caWallet":{"getWallet": 2}}' \
  http://localhost:8080/api/new/contract/activate | jq
```

From these two queries you will get back two contract instance IDs. These will be needed
in the subsequent steps for running actions against. We can optionally take a look at the state
of the contract with the `status` API:

2. Get the status

```
export INSTANCE_ID=...
curl -s http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/status | jq
```

This has a lot of information; and in particular we can see what endpoints are still available
to call.

3. Start the game by locking some value inside

Now, let's call the `lock` endpoint to start the game. In order to do so, we need to construct
a JSON representation of the `LockParams` that the endpoint takes (look at `Game.hs`). The easiest
way is to simply build the term in haskell and ask `aeson` to encode it. From the terminal:

```
cabal repl

import Contracts.Currency
import Ledger.Value   
import Data.Aeson
import Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as B
args = SimpleMPS { Contracts.Currency.tokenName = TokenName $ B.pack "testToken", amount = 10 }
BSL.putStrLn $ encode args

{"tokenName":{"unTokenName":"testToken"},"amount":10}
```

Great! This is all we need to call the `lock` endpoint, so let's do that now with
the instance from Wallet 1:

4. Lock some value (Wallet 1)

```
export INSTANCE_ID=...
curl -H "Content-Type: application/json" \
  --request POST \
  --data '{"tokenName":{"unTokenName":"testToken"},"amount":10}' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/createNativeToken
```

We can do likewise to work out what the JSON for `GuessParams` is, and then make a guess from
Wallet 2:

5. Make a guess (Wallet 2)

```
export INSTANCE_ID=...
curl -H "Content-Type: application/json" \
  --request POST \
  --data '{"guessWord": "duck"}' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/guess
```


curl -H "Content-Type: application/json" \
  --request POST \
  --data '{"guessWord": "eagle"}' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/guess
Note that this guess is wrong, so in the log of the server we will see that the transaction
didn't validate.

As an exercise, you can now spin up another instance for Wallet 2 and make a correct guess, and
confirm that the transaction validates and the Ada is transferred into the right wallet.

Note that you can verify the balances by looking at the log of `plutus-starter-pab` 
when exiting it by pressing return.

Finally, also node that the PAB also exposes a websocket, which you can read about in
the general [PAB Architecture documentation](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).