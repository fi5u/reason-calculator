[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo: string = "./logo.svg";

type state = {userInputValue: Input.inputValue};

type action =
  | UpdateValue(Input.inputValue);

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,

  initialState: () => {userInputValue: "0"},

  reducer: (action, _state) =>
    switch (action) {
    | UpdateValue(value) => ReasonReact.Update({userInputValue: value})
    },

  render: self =>
    <div className="App">
      <h3> {ReasonReact.string(self.state.userInputValue)} </h3>
      <p> {ReasonReact.string("Enter a sum")} </p>
      <Input onSubmit={value => self.send(UpdateValue(value))} />
    </div>,
};