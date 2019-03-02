[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo: string = "./logo.svg";

type total = int;
type sumItem = {v: string};
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
          },
        values:
          switch (state.values) {
          | None => Some([{v: value}])
          | Some(values) => Some([{v: value}, ...values])
          },
      })
    },

  render: self => {
    <div className="App">
      <div>
        {switch (self.state.values) {
         | None => ReasonReact.string("Nothing yet...")
         | Some(values) =>
           switch (values) {
           | [] => ReasonReact.string("Empty list...")
           | _ =>
             ReasonReact.array(
               Array.of_list(
                 List.rev(
                   List.map(
                     item => <PrevValue key={item.v} value={item.v} />,
                     values,
                   ),
                 ),
               ),
             )
           }
         }}
      </div>
      <div>
        {switch (self.state.total) {
         | 0 => ReasonReact.null
         | _ => ReasonReact.string(string_of_int(self.state.total))
         }}
      </div>
      <p> {ReasonReact.string("Enter a sum")} </p>
      <Input onSubmit={value => self.send(UpdateValue(value))} />
    </div>;
  },
};