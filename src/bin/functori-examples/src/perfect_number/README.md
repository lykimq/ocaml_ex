# README

This README was generated with your project. It is intended to help use factori in both [Typescript](#typescript) and [OCaml](#ocaml).

## Typescript

The easiest way to try factori if you don't have your own contract at hand is to import an existing contract from the blockchain. We choose a FA2 contract on the Ithacanet blockchain.

```shell=
factori import kt1 . KT1ATZMPus96CFMg2s7mHp33bVkHwwpRDS3R --name fa2 --network ithacanet --typescript --force
**Project 'my-factori-project' created.
tree -P '*.ml|dune|functolib.ts|imported*.ts|*_interface.ts' -I '_build|_opam' --prune /tmp/test_factori/tmp/test_factori
└── src
    └── ts-sdk
        └── functolib.ts

2 directories, 1 file
replaced /tmp/test_factori/src/ts-sdk/fa2_interface.ts

Import of KT1ATZMPus96CFMg2s7mHp33bVkHwwpRDS3R successful.
/tmp/test_factori
└── src
    └── ts-sdk
        └── fa2_interface.ts

2 directories, 1 file
```
### Inventory of the generated interface

The file `fa2_interface.ts` contains a lot of functions. Note that for every type it identified from the Michelson contract, it systematically generated:

- a Typescript type (e.g. `storage`)
- an encoder from the Typescript type to the corresponding Michelson type (e.g. `storage_encode`)
- a decoder from Michelson to the Typescript type (e.g. `storage_decode`)

For every entrypoint of the contract, it also generated a calling function. And, of course, a deploying function.


### Write a scenario

Let's write a small scenario (`<dir>/src/ts-sdk/scenario.ts`) which we will run on the `flextesa` sandbox.

First, as always in Typescript, we need a small header:

```typescript=
import {
    TezosToolkit,
    MichelsonMap,
  } from "@taquito/taquito";
import { MichelsonV1Expression } from "@taquito/rpc"
import * as fa2 from "./fa2_interface";
import {big_map,setSigner, config, alice_flextesa, bob_flextesa, wait_inclusion } from "./functolib";
const tezosKit = new TezosToolkit(config.node_addr);
```

To make the storage a little easier to write, let's write a small helper function for building empty big_maps:

```typescript=
function empty_big_map <K,V>(){
    let res : big_map<K,V> = {kind : 'literal',value : []}
    return(res)
}
```
We are ready to write our `main` function. You can try to fill out the initial storage yourself by looking at the `storage` type in `fa2_interface.ts`.

```typescript=
async function main_fa2(tezosKit: TezosToolkit) {
    // Our deployer will be Alice, a pre-filled account on Flextesa
    setSigner(tezosKit, alice_flextesa.sk);
    // Our initial storage
    let init_st : fa2.storage = {
      metadata : empty_big_map(),
      assets : [
                [
                  [ empty_big_map(), empty_big_map()],
                  [ {kind: "Owner_or_operator_transfer_constructor"},
                    {kind: "Optional_owner_hook_constructor"},
                    {kind: "Optional_owner_hook_constructor"},
                    {config_api : alice_flextesa.pkh,tag : "TODO"}],
                  empty_big_map()
                ],
                BigInt(0)
              ],
       admin: [[alice_flextesa.pkh, false], null] };
    // All that remains to do is deploy the contract and get its KT1 back
    let kt1 = await fa2.deploy_fa2(tezosKit,init_st)
    console.log(`KT1 : ${kt1}`)
    }

  main_fa2(tezosKit)
```

### Compiling the scenario

In order to compile the scenario, you need to first install the typescript dependencies from the root of your project and compile your SDK:

```shell
make ts-deps
```

Now you can compile your scenario:
```shell=
cd src/ts-sdk
tsc scenario.ts
```

### Get Flextesa running

Then, get the flextesa sandbox running. If this is your first time, it may take several minutes and will use some bandwidth, but the next times it will be instantaneous.
```shell=
image=oxheadalpha/flextesa:latest
script=ithacabox
docker run --rm --name my-sandbox --detach -p 20000:20000 -e block_time=1 "$image" "$script" start
```
### Run the scenario

Once the flextesa sandbox is running:
```shell
$ node scenario.js
```
will run your scenario:
```shell=
$ node scenario.js
main from fa2_interface
[deploy_fa2_raw] Deploying new fa2 smart contract
Waiting for confirmation of origination for KT1Wvwo1xrJAqNmasH1dfDp3gA7fNMhkJKVb...
Origination completed.
```

### Calling the deployed contract

#### Update_operator

Now, let's call our contract, back to the `main` function. Let's say Alice wants to hand Bob control over her account by calling the `update_operator` feature of the FA2.

```typescript=
let param_update_operators : fa2.update_operators = [{kind : "Add_operator_constructor", Add_operator_element : {token_id : BigInt(0),operator : bob_flextesa.pkh, owner : alice_flextesa.pkh}}]
    //Alice is the signer of this transaction
    setSigner(tezosKit, alice_flextesa.sk);
    let update_operator_op = await fa2.call_update_operators(tezosKit,kt1,param_update_operators)
    await wait_inclusion(update_operator_op);
    console.log(`update operator_op hash: ${update_operator_op.hash}`)
```

#### Mint

But Alice doesn't have any money! We want to mint her some tokens:
```typescript=
    let mint_param : fa2.mint_tokens = [{amount : BigInt(10),owner : alice_flextesa.pkh}]
    let mint_op = await fa2.call_mint_tokens(tezosKit,kt1,mint_param)
    await wait_inclusion(mint_op)
    console.log(`mint hash: ${mint_op.hash}`)
```

#### Transfer

Now Bob can send himself some money from Alice's account:

```typescript=
    let param_transfer : fa2.transfer = [{txs : [{token_id : BigInt(0),amount : BigInt(1), to_ : bob_flextesa.pkh}],from_ : alice_flextesa.pkh}]
    // We change the signer to Bob
    setSigner(tezosKit,bob_flextesa.sk)
    let op_transfer = await fa2.call_transfer (tezosKit,kt1,param_transfer)
    console.log(`send op_hash: ${JSON.stringify(op_transfer)}`)
```

### Run the complete `main` function

If we run our main function again, after a re-deployment we read:

```shell=
[wait_inclusion] Waiting inclusion of operation ong4DLA72EqRaxKzWhAdp9GYdk6F2ZDbS3sxRmdCwv2qxd8f36L at level 13
Waiting inclusion ... block level is 13
Waiting inclusion ... block level is 14
update operator_op hash: ong4DLA72EqRaxKzWhAdp9GYdk6F2ZDbS3sxRmdCwv2qxd8f36L
[wait_inclusion] Waiting inclusion of operation oosVXoT24HrHXrEH8wep1FpqxgQSFd9y2NG5Kvrkyh6JkPSghka at level 14
Waiting inclusion ... block level is 14
Waiting inclusion ... block level is 15
mint hash: oosVXoT24HrHXrEH8wep1FpqxgQSFd9y2NG5Kvrkyh6JkPSghka
send op_hash: {"hash":"ooCKqhcV9jRiNAEpt8U5jZjdYAqJjvTsUZ2JEJZZGvwcmzEa5Pe","level":15,"error":null}
```

## OCaml

For each contract <contract> imported into Factori, you will find

- A file ./src/ocaml_sdk/<contract>_ocaml_interface.ml ;
- A file containing the contract's Michelson code ./src/ocaml_sdk/<contract>_code.ml ;
- A scenario example file ./src/ocaml_scenarios/scenario.ml.example. Note that this file will be overwritten when you import the contract again.

In this last file, you will find initial storage pulled from the
blockchain for every contract you imported from the blockchain. Note that
big_maps will look like `Abstract (Z.of_string "42")`, that's because on
the blockchain, big_maps are stored as their index in the blockchain.
You will not be able to deploy them like this, you need to replace them
with a literal big_map, such as:
```ocaml
mybigmap = Literal ["key1","value1";"key2","value2"]
```
with the right types for keys and values (here we used `string` for both).
You can find these easily by looking up the type of `mybigmap` in the
contract's OCaml SDK, which is easy to do with Merlin.

### Common contracts:

1. If you want to import a FA2, you can use any FA2 that's deployed on the blockchain, such as e.g. KT1TwzD6zV3WeJ39ukuqxcfK2fJCnhvrdN1X (chosen at random) :
```shell
factori import kt1 . KT1TwzD6zV3WeJ39ukuqxcfK2fJCnhvrdN1X --name fa2 --ocaml
```
2. Same for a FA1.2 :
```shell
factori import kt1 . KT1LwPxVGwMAugQBtA7rz9CcNpbGsMyWraz8 --name fa12 --ocaml
```

### Deploying your contract

Once you have defined a storage (for example the `initial_storage`
found in ./src/ocaml_scenarios/scenario.ml.example) you can deploy your contract to any blockchain you wish.
We give a full example here with the FA12 contract above.

```ocaml
open Tzfunc.Rp
open Fa12_ocaml_interface

let initial_storage_fa12 =
{administrator = "tz1W18LSYogRApX5MBnsr7KupHEc9rC8vDvz";
balances = Literal [];
metadata = Literal [];
paused = false;
token_metadata = Literal [];
totalSupply = Z.of_string ("1000000")}

let _ = Tzfunc__Node.set_silent true (* optional, this is to remove all the request traces from the standard output *)
let main () =
  let>? kt1 = Fa12_ocaml_interface.deploy
    ~node:Blockchain.sandbox_node (* change this if you want another network *)
    ~from:Blockchain.bootstrap1
    initial_storage_fa12 in
  Lwt.return_ok @ Format.eprintf "Contract deployed with KT1: %s" kt1

let _ = Lwt_main.run (main ())

```

Now if you actually want to run your scenario, go to the project root and type:
```shell
make run_scenario_ocaml
```

If you don't want to use the preset identities bootstrap1,
bootstrap2 etc..., you can also define your own identities
(filling with suitable matching secret key, publick key and
public key hash):

```ocaml
let agent : identity =
  {
    pkh = "tz1id1nnbxUwK1dgJwUra5NEYixK37k6hRri";
    pk = "edpkv2Gafe23nz4x4hJ1SaUM3H5hBAp5LDwsvGVjPsbMyZHNomxxQA";
    sk = "edsk4LhLg3212HzL7eCXfCWWvyWFDfwUS7doL4JUSmkgTe4qwZbVYm";
  }
```
### Calling an entrypoint

All entrypoints <ep> have a corresponding function call_<ep>. For instance
we can call the entrypoint `approve` of the FA12, which we add to our
main function from above:

```ocaml
let main () =
  let>? kt1 =
     [...]
  let>? op_hash = call_approve ~from:Blockchain.bootstrap1 ~kt1:kt1
                    {value = (Z.of_int 10);
                     spender = Blockchain.bootstrap1.pkh} in
  Lwt.return_ok @ Format.eprintf "Contract deployed with KT1: %s\nApprove entrypoiny called, op_hash of operation is %s" kt1 op_hash

```

Feel free to explore the Blockchain module, where you will find
many other things you can do with your smart contracts.

