type inputValue = string;
type state = inputValue;

type lastCharacter =
  | Empty
  | Digit
  | Math(char);

/**
 * Get string value from input event
 */
let getValueFromEvent = (evt): string =>
  (evt |> ReactEvent.Form.target)##value;

/**
 * Is the passed char a math char
 */
let isMathChar = char =>
  switch (char) {
  | '+'
  | '-'
  | '*' => true
  | _ => false
  };

/**
 * Is the passed char a number
 */
let isNumber = char => int_of_char(char) >= 48 && int_of_char(char) <= 57;

/**
 * Handle text input changes
 */
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
  | Digit =>
    // Update input element with string
    send(string)
  | Math(last) =>
    // Update input element with string
    send(string);
    // Send the whole string back
    onSubmit(String.sub(string, 0, String.length(string) - 1));
    // Convert last char to string and update input element
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
      placeholder="Type a calculation"
      onChange={evt => charControl(send, onSubmit, getValueFromEvent(evt))}
      onKeyDown={evt =>
        if (ReactEvent.Keyboard.key(evt) == "Enter") {
          onSubmit(text);
          send("");
        }
      }
    />,
};