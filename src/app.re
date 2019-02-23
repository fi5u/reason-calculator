[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo: string = "./logo.svg";

type sumItem = {v: string};
type userValues = list(sumItem);

type state = {
  userInputValue: Input.inputValue,
  values: option(userValues),
};

type action =
  | UpdateValue(Input.inputValue);

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,

  initialState: () => {userInputValue: "0", values: Some([])},

  reducer: (action, state) =>
    switch (action) {
    | UpdateValue(value) =>
      ReasonReact.Update({
        userInputValue: value,
        values:
          switch (state.values) {
          | None => state.values
          | Some(values) => Some([{v: value}, ...values])
          },
      })
    },

  render: self =>
    <div className="App">
      <h3> {ReasonReact.string(self.state.userInputValue)} </h3>
      <div>
        {switch (self.state.values) {
         | None => ReasonReact.string("Nothing yet...")
         | Some(values) =>
           switch (values) {
           | [] => ReasonReact.string("Zilch...")
           | _ =>
             ReasonReact.array(
               Array.of_list(
                 List.map(
                   item => <PrevValue key={item.v} value={item.v} />,
                   values,
                 ),
               ),
             )
           }
         }}
      </div>
      <p> {ReasonReact.string("Enter a sum")} </p>
      <Input onSubmit={value => self.send(UpdateValue(value))} />
    </div>,
};