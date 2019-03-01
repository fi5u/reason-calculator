type inputValue = string;
type state = inputValue;

type lastCharacter =
  | Empty
  | Digit
  | Math(char);

let valueFromEvent = (evt): string => (evt |> ReactEvent.Form.target)##value;

let isMathChar = char =>
  switch (char) {
  | '+'
  | '-' => true
  | _ => false
  };

let isNumber = char => int_of_char(char) >= 48 && int_of_char(char) <= 57;

let charControl = (send, onSubmit, string) => {
  let lastChar =
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

  switch (lastChar) {
  | Empty => send("")
  | Digit => send(string)
  | Math(last) =>
    send(string);
    onSubmit(String.sub(string, 0, String.length(string) - 1));
    send(String.make(1, last));
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
        charControl(send, onSubmit, inputValue);
      }}
      onKeyDown={evt =>
        if (ReactEvent.Keyboard.key(evt) == "Enter") {
          onSubmit(text);
          send("");
        }
      }
    />,
};