[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo: string = "./logo.svg";

type total = int;
type sumItem = {
  m: option(char),
  v: string,
};
type userValues = list(sumItem);

type state = {
  total,
  values: option(userValues),
};

type action =
  | UpdateValue(Input.inputValue);

/**
 * Calculate two values with a math symbol
 */
let calculateValues = (v1, v2, math) =>
  switch (math) {
  | '+' => v1 + v2
  | '-' => v1 - v2
  | '*' => v1 * v2
  | _ => 0
  };

/**
 * Get the total of `r` and value of current element
 */
let getTotal = (r, elem) =>
  switch (elem.m) {
  | None => r + int_of_string(elem.v)
  | Some(math) => calculateValues(r, int_of_string(elem.v), math)
  };

/**
 * Process the input value to extract possible math symbol
 * and value
 */
let processInput = input =>
  if (Input.isMathChar(input.[0])) {
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

  initialState: () => {total: 0, values: None},

  reducer: (action, state) =>
    switch (action) {
    | UpdateValue(value) =>
      ReasonReact.Update({
        total:
          switch (state.values) {
          | None => 0
          | Some(values) =>
            calculateValues(
              ListLabels.fold_left(~f=getTotal, ~init=0, values),
              int_of_string(String.sub(value, 1, String.length(value) - 1)),
              value.[0],
            )
          },
        values:
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
        {switch (self.state.total) {
         | 0 => ReasonReact.null
         | _ =>
           ReasonReact.string("Total:" ++ string_of_int(self.state.total))
         }}
      </div>
      <Input
        onSubmit={value => self.send(UpdateValue(value))}
        valuesLength={
          switch (self.state.values) {
          | None => 0
          | Some(values) => List.length(values)
          }
        }
      />
    </div>;
  },
};