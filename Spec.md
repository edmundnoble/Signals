# RxPebble Specfication

## Semantics

### Language Elements
Everything that is RxPebble can be summed up in one word: _signals_.

#### Signals
_Signals_ in RxPebble are push-based streams of data which can be consumed by the update_proc of a Pebble Layer. Signals all have types analogous to the Pebble C SDK types they represent. New signals can be declared using the __signal__ keyword.

```
signal hour_angle : uint64_t
```

Some signals, like the current time, are built in to RxPebble.

#### Types
All of the primitive types of RxPebble are the types exposed by the Pebble C SDK. Type aliases can be employed to give an extant type a new name, using the __type__ keyword.

```
type Word = int16_t
```

New types (structure types) can be also declared using the __type__ keyword.

```
type LayerWithColor = {
	Layer layer
	GColor color
}
```

#### Layers
RxPebble can be used to define custom Pebble Layers and their `update_proc` callbacks. The code inside the update_proc callback is a super-set of C as according to the C11 standard. Signals can be referred to from the `update_proc` definition using the `${...}` notation as shown below. 

```
signal circle_radius : uint64_t
layer clock_layer = (ctx) => {
	graphics_draw_circle(ctx, ${circle_radius});
} end
```

#### Constants
Truly compile-time constant values, values that must be initialized and cleaned up during `window_load()` and `window_unload()`, and values that must be initialized during `init()` and cleaned up during `deinit()` are all considered constants in RxPebble.

## Syntax

All identifiers are case-sensitive.

 _program ::= (statement wsp? newline wsp?)+_
   
 _statement ::= (signal-declaration | type-declaration | layer-declaration | constant-declaration)_
 
 _type-declaration ::= "type" wsp type-name wsp "=" wsp (type-alias-declaration | type-struct-declaration)_
 
 _type-alias-declaration ::= type-name_
 
 _type-struct-declaration ::= "{" wsp? (type-struct-field newline)* wsp? "}"I_
 
 _type-struct-field ::= type-name field-name_
 
 _layer-declaration ::= "layer" wsp layer-name wsp? "=" wsp? draw-proc-definition_
 
 _draw-proc-definition ::= "(" id ")" wsp? "=>" wsp? c-literal_
 
 _c-literal ::= "{" wsp? c-code wsp? "} end"_
 
 _type-name ::= id_
 
 _field-name ::= id_
 
 _id ::= regex[a-zA-Z0-9]+_
 
 _newline ::= "\n"_