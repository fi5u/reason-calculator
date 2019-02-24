type inputValue = string;
type state = inputValue;

let valueFromEvent = (evt): string => (evt |> ReactEvent.Form.target)##value;

let isMathChar = char =>
  switch (char) {
  | '+'
  | '-' => true
  | _ => false
  };

let isNumber = char => int_of_char(char) >= 48 && int_of_char(char) <= 57;

let charControl = (send, string) => {
  let lastChar = string.[String.length(string) - 1];
  if (isNumber(lastChar)) {
    send(string);
  };
};

let component = ReasonReact.reducerComponent("Input");

let make = (~onSubmit, _) => {
  ...component,

  initialState: () => "",

  reducer: (newText, _text) => ReasonReact.Update(newText),

  render: ({state: text, send}) =>
    <input
      value=text
      type_="text"
      placeholder="Write something to do"
      onChange={evt => {
        let inputValue = ReactEvent.Form.target(evt)##value;
        charControl(send, inputValue);
      }}
      onKeyDown={evt =>
        if (ReactEvent.Keyboard.key(evt) == "Enter") {
          onSubmit(text);
          send("");
        }
      }
    />,
};