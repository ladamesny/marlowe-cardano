exports.setupTestApp = (element) => () => {
  element.innerHTML = `
    <p id="para" role="paragraph">Test content</p>
    <button />
  `;
  element.querySelector("button").addEventListener("click", () => {
    element.querySelector("#para").textContent = "It worked!";
  });
};