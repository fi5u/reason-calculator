open Helpers;

[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo: string = "./logo.svg";

type sumItem = {
  m: option(char),
  v: string,
};
type userValues = list(sumItem);

type lastCharacter =
  | Empty
  | Digit
  | Math(char);

type activeIndex = int;

type state = {
  activeIndex: option(activeIndex),
  inputValue: string,
  values: option(userValues),
};

type action =
  | ActivateValue(int)
  | UpdateInput(string)
  | UpdateValue(Input.inputValue);

let mylist = ["a", "b", "d"];
let newVar = "c";
Js.log(List.concat([['x', 'y'], ['z']]));
Js.log(List.concat([slice(mylist, 0, 2), [newVar], slice(mylist, 2, 3)]));

/**
 * Calculate two values with a math symbol
 */
let calculateValues = (v1, v2, math): float => {
  switch (math) {
  | '+' => v1 +. v2
  | '-' => v1 -. v2
  | '*' => v1 *. v2
  | '/' => v1 /. v2
  | _ => 0.0
  };
};

/**
 * Iterator to get the total of `r` and value of current element
 */
let getTotalIterator = (r, elem) =>
  switch (elem.m) {
  | None => r +. float_of_string(elem.v)
  | Some(math) => calculateValues(r, float_of_string(elem.v), math)
  };

/**
 * Iterate over values to calculate total
 */
let getTotal = values =>
  ListLabels.fold_left(~f=getTotalIterator, ~init=0.0, List.rev(values));

/**
 * Is the passed char a math char
 */
let isMathChar = char =>
  switch (char) {
  | '+'
  | '-'
  | '*'
  | '/' => true
  | _ => false
  };

/**
 * Is the passed char a number
 */
let isNumber = char => int_of_char(char) >= 48 && int_of_char(char) <= 57;

/**
 * Get the type of last char in string
 */
let getLastCharType = (string): lastCharacter =>
  if (String.length(string) === 0) {
    Empty;
  } else {
    let last = string.[String.length(string) - 1];
    if (isNumber(last)) {
      Digit;
    } else if (isMathChar(last)) {
      Math(last);
    } else {
      Empty;
    };
  };

/**
 * Process the input value to extract possible math symbol
 * and value
 */
let processInput = input =>
  if (isMathChar(input.[0])) {
    {
      m: Some(input.[0]),
      v: String.sub(input, 1, String.length(input) - 1),
    };
  } else {
    {m: None, v: input};
  };

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,

  initialState: () => {activeIndex: None, inputValue: "", values: None},

  reducer: (action, state) =>
    switch (action) {
    // SELECT A VALUE TO EDIT
    | ActivateValue(index) =>
      ReasonReact.Update({
        ...state,
        activeIndex:
          switch (index) {
          | activeIndex => Some(activeIndex)
          },
        inputValue:
          switch (state.values) {
          | None => ""
          | Some(values) =>
            // If has a math value, append that
            switch (List.nth(values, index).m) {
            | None => List.nth(values, index).v
            | Some(mathValue) =>
              String.make(1, mathValue) ++ List.nth(values, index).v
            }
          },
      })

    // INPUT VALUE UPDATES:
    | UpdateInput(inputString) =>
      let lastCharType = getLastCharType(inputString);

      ReasonReact.Update({
        ...state,
        inputValue:
          switch (lastCharType) {
          | Empty => ""
          | Digit => inputString
          | Math(mathChar) => String.make(1, mathChar)
          },
        values:
          switch (lastCharType) {
          | Empty => state.values
          | Digit => state.values
          | Math(_) =>
            // Test for string length, if only one, and is math char,
            // then don't save yet (allows for minus numbers)
            switch (String.length(inputString)) {
            | 1 => state.values
            | _ =>
              switch (state.values) {
              | None =>
                Some([
                  {
                    m: None,
                    v:
                      // Strip the last math char
                      String.sub(
                        inputString,
                        0,
                        String.length(inputString) - 1,
                      ),
                  },
                ])
              | Some(values) =>
                Some([
                  {
                    m: Some(inputString.[0]),
                    v:
                      // Strip the first + last math chars
                      String.sub(
                        inputString,
                        1,
                        String.length(inputString) - 2,
                      ),
                  },
                  ...values,
                ])
              }
            }
          },
      });

    // ENTER PRESSED
    | UpdateValue(value) =>
      ReasonReact.Update({
        ...state,
        inputValue: "",
        values:
          // If a value has been selected, replace the value
          switch (state.activeIndex) {
          | Some(index) =>
            switch (state.values) {
            | None => None // This case should be impossible
            | Some(values) =>
              Some(
                List.concat([
                  slice(values, 0, index),
                  [processInput(value)],
                  slice(values, index + 1, -1),
                ]),
              )
            }
          | None =>
            // Otherwise update values with new value
            switch (state.values) {
            | None => Some([{m: None, v: value}])
            | Some(values) =>
              Some([
                {
                  m: Some(value.[0]),
                  v: String.sub(value, 1, String.length(value) - 1),
                },
                ...values,
              ])
            }
          },
      })
    },

  render: self => {
    <div className="App">
      <table>
        <tbody>
          {switch (self.state.values) {
           | None =>
             <tr> <td> {ReasonReact.string("Nothing yet...")} </td> </tr>
           | Some(values) =>
             switch (values) {
             | [] =>
               <tr> <td> {ReasonReact.string("Empty list...")} </td> </tr>
             | _ =>
               ReasonReact.array(
                 Array.of_list(
                   List.rev(
                     List.mapi(
                       (i, item) =>
                         <PrevValue
                           key={
                             // Ensure two keys do not have same value
                             string_of_int(i) ++ "-" ++ item.v
                           }
                           math={item.m}
                           onClick={_ => self.send(ActivateValue(i))}
                           value={item.v}
                         />,
                       values,
                     ),
                   ),
                 ),
               )
             }
           }}
        </tbody>
      </table>
      <div>
        {switch (self.state.values) {
         | None => ReasonReact.string("Total: 0")
         | Some(vals) =>
           let total = getTotal(vals);
           let totalString = string_of_float(total);
           let lastChar = string_of_float(total).[String.length(totalString)
                                                   - 1];
           // Only show decimal point if number has a remainder
           switch (lastChar) {
           | '.' =>
             ReasonReact.string(
               "Total: " ++ string_of_int(int_of_float(total)),
             )
           | _ => ReasonReact.string("Total: " ++ string_of_float(total))
           };
         }}
      </div>
      <Input
        onChange={value => self.send(UpdateInput(value))}
        onSubmit={value => self.send(UpdateValue(value))}
        value={self.state.inputValue}
      />
      <div>
        {switch (self.state.activeIndex) {
         | None => ReasonReact.string("")
         | Some(activeIndexValue) =>
           ReasonReact.string(
             "Selected: " ++ string_of_int(activeIndexValue),
           )
         }}
      </div>
    </div>;
  },
};