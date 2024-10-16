// Function to handle clicks on SVG elements
const handleSvgClick = event => {
    const target = event.target.closest('[id]')
    if (!target) {
        return
    }
    console.log('keycode', target.getAttribute('data-keycode'))
    target.classList.toggle('black')
    setTimeout(() => target.classList.toggle('black'), 100)
    if (target.classList.contains('led-key')) {
        const dataGroup = target.getAttribute('data-group')

        if (dataGroup) {
            if (!target.querySelector('.led').classList.contains('active')) {
                const groupElements = target.ownerDocument.querySelectorAll(`.led-key[data-group="${dataGroup}"]`)
                groupElements.forEach(element => {
                    element.querySelector('.led').classList.remove('active')
                })
                target.querySelector('.led').classList.toggle('active', true)
            }
        } else {
            target.querySelector('.led').classList.toggle('active')
        }
    }
}

// Function to set the active state of an element's LED child
const setLedActive = (elementId, isActive) => {
    const svgDoc = document.getElementById('svg-object').contentDocument
    const element = svgDoc.getElementById(elementId)
    if (element) {
        const led = element.querySelector('.led')
        if (led) {
            led.classList.toggle('active', isActive)
        }
    }
}

// Add event listener to the embedded SVG once it's loaded
document.getElementById('svg-object').addEventListener('load', () => {
    const svgDoc = document.getElementById('svg-object').contentDocument
    svgDoc.addEventListener('click', handleSvgClick)
})

const resizeKeyboard = () => {
  const keyboardContainer = document.getElementById('keyboard-container');
  const svgObject = document.getElementById('svg-object');
  const aspectRatio = svgObject.getAttribute('width') / svgObject.getAttribute('height');
  const containerWidth = keyboardContainer.clientWidth;
  const containerHeight = window.innerHeight * (1 / 3); // One-third of the window height

  let width, height;
  if (containerWidth / containerHeight > aspectRatio) {
    // Window is too wide, adjust width to maintain aspect ratio
    height = containerHeight;
    width = height * aspectRatio;
  } else {
    // Window is not too wide, adjust height to maintain aspect ratio
    width = containerWidth;
    height = width / aspectRatio;
  }

  // Apply the new size to the SVG object
  svgObject.style.width = `${width}px`;
  svgObject.style.height = `${height}px`;

  // Center the SVG object horizontally if the window is too wide
  svgObject.style.marginLeft = `${(containerWidth - width) / 2}px`;
  svgObject.style.marginRight = `${(containerWidth - width) / 2}px`;
  svgObject.style.marginTop = '0';
  svgObject.style.marginBottom = '0';
};

document.getElementById('svg-object').addEventListener('load', () => {
  resizeKeyboard();
  const svgDoc = document.getElementById('svg-object').contentDocument;
  svgDoc.addEventListener('click', handleSvgClick);
});

window.addEventListener('resize', resizeKeyboard);
resizeKeyboard();
