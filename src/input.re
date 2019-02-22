type inputValue = string;
type state = inputValue;

let valueFromEvent = (evt): string => (evt |> ReactEvent.Form.target)##value;

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
      onChange={evt => send(ReactEvent.Form.target(evt)##value)}
      onKeyDown={evt =>
        if (ReactEvent.Keyboard.key(evt) == "Enter") {
          onSubmit(text);
          send("");
        }
      }
    />,
};