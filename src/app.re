[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo: string = "./logo.svg";

type sumItem = {v: string};
type userValues = list(sumItem);

type state = {values: option(userValues)};

type action =
  | UpdateValue(Input.inputValue);

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,

  initialState: () => {values: None},

  reducer: (action, state) =>
    switch (action) {
    | UpdateValue(value) =>
      ReasonReact.Update({
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
      <p> {ReasonReact.string("Enter a sum")} </p>
      <Input onSubmit={value => self.send(UpdateValue(value))} />
    </div>;
  },
};