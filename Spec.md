# RxPebble Specfication

## Semantics

### Language Elements
Everything that is RxPebble can be summed up in one word: _signals_.

#### Signals
_Signals_ in RxPebble are push-based streams of data which can be consumed by the update_proc of a Pebble Layer. Signals all have types analogous to the Pebble C SDK types they represent. New signals can be declared using the __signal__ keyword.

```
signal uint64_t hour_angle;
```

The behavior that generates signals is contained in the _Stage_.

#### Stage
The _stage_ is an area of an RxPebble program which describes the behavior of the program's signals. 

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
RxPebble can be used to define custom Pebble Layers and their `update_proc` callbacks. The code inside the update_proc callback is a super-set of C as according to the C11 standard. Signals can be referred to directly from the `update_proc` definition as shown below. 

```
signal uint64_t circle_radius;

layer clock_layer = (ctx) => {
	graphics_draw_circle(ctx, circle_radius);
}
```

#### Constants
Truly compile-time constant values, values that must be initialized and cleaned up during `window_load()` and `window_unload()`, and values that must be initialized during `init()` and cleaned up during `deinit()` are all considered constants in RxPebble.

## Syntax

All identifiers are case-sensitive.

 _program ::= (statement wsp? newline wsp?)+_
   
 _statement ::= (signal-declaration | type-declaration | layer-declaration | constant-declaration | animation-declaration)_
 
 _signal-declaration ::= "signal" wsp type-name wsp signal-name wsp? ";"_
 
 _type-declaration ::= "type" wsp type-name wsp "=" wsp (type-alias-declaration | type-struct-declaration)_
 
 _type-alias-declaration ::= type-name_
 
 _type-struct-declaration ::= "{" wsp? (type-struct-field newline)* wsp? "}"_
 
 _type-struct-field ::= type-name field-name_
 
 _layer-declaration ::= "layer" wsp layer-name wsp? "=" wsp? draw-proc-definition_
 
 _draw-proc-definition ::= "(" id ")" wsp? "=>" wsp? c-literal_
 
 _animation-curve ::= "linear" | "ease\_out" | "ease\_in" | "ease\_in\_out"_
 
 _animation-definition ::= animation-curve wsp "from" wsp duration wsp ("after" duration wsp)? "{" newline_
 
 _animation-declaration ::= "then"? animation-curve wsp "from" wsp duration wsp ("after" duration wsp)? "{" newline wsp? (signal-name "from" wsp start-value "to" wsp end-value)*(min = 1, sep = wsp? "," newline) newline "}"_
 
 ```
 signal GRect sig_rect;
 signal GColor8 sig_color;
 
stage {
  intro {
    ease_in_out for 1000ms after 100ms {
      sig_rect from GRect(GPoint(0, 0), GSize(180, 180))
               to GRect(GPoint(180, 180), GSize(0, 0))
    } then linear for 1000ms after 100ms {
      sig_rect from GRect(GPoint(180, 180), GSize(0, 0))
               to GRect(GPoint(0, 0), GSize(180, 180))
    }
    linear for 2000ms {
      sig_color = GColorBlue;
    } then linear for 1000ms {
      sig_color = GColorGreen;
    }
  }
  forever {
  	sig_color = GColorBlue;
  	sig_rect = GPoint(tick_time->tm_hour, tick_time
  }
}
 
 layer clock_layer = (ctx) => {
   graphics_fill_rect(ctx, sig_rect);
 }
 ```
 
 _c-literal ::= "{" wsp? c-code wsp? "}"_
 
 _type-name ::= id_
 
 _field-name ::= id_
 
 _signal-name ::= id_
 
 _id ::= regex { [a-zA-Z0-9]+ }_
 
 _duration ::= regex { -?[0-9]+ } wsp? "ms"_
 
 _newline ::= wsp? "\n" wsp?_