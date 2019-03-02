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

let getTotal = (r, elem) => r + int_of_string(elem.v);

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
            ListLabels.fold_left(~f=getTotal, ~init=0, values)
            + int_of_string(value)
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
                     List.map(
                       item =>
                         <PrevValue
                           key={item.v}
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
      <p> {ReasonReact.string("Enter a sum")} </p>
      <Input onSubmit={value => self.send(UpdateValue(value))} />
    </div>;
  },
};