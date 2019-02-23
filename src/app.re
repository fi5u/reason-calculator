[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo: string = "./logo.svg";

type sumItem = {value: string};

type state = {
  userInputValue: Input.inputValue,
  values: option(list(sumItem)),
};

type action =
  | UpdateValue(Input.inputValue);

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,

  initialState: () => {
    userInputValue: "0",
    values: Some([{value: "123456"}, {value: "654321"}]),
  },

  reducer: (action, state) =>
    switch (action) {
    | UpdateValue(value) =>
      ReasonReact.Update({...state, userInputValue: value})
    },

  render: self =>
    <div className="App">
      <h3> {ReasonReact.string(self.state.userInputValue)} </h3>
      <div>
        {switch (self.state.values) {
         | None => ReasonReact.string("Nothing yet...")
         | Some(values) =>
           ReasonReact.array(
             Array.of_list(
               List.map(item => <PrevValue value={item.value} />, values),
             ),
           )
         }}
      </div>
      <p> {ReasonReact.string("Enter a sum")} </p>
      <Input onSubmit={value => self.send(UpdateValue(value))} />
    </div>,
};