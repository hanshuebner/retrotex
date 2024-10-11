// Function to handle clicks on SVG elements
const handleSvgClick = event => {
    const target = event.target.closest('[id]')
    if (target && target.classList.contains('led-key')) {
        const svgDoc = target.ownerDocument
        const dataGroup = target.getAttribute('data-group')

        if (!target.classList.contains('active')) {
            // Remove 'active' class from all elements with the same data-group attribute
            const groupElements = svgDoc.querySelectorAll(`.led-key[data-group="${dataGroup}"]`)
            groupElements.forEach(element => element.classList.remove('active'))

            // Add 'active' class to the clicked element
            target.classList.add('active')
        }
    }
}

// Function to set the active state of an element's LED child
const setLedActive = (elementId, isActive) => {
    const svgObject = document.getElementById('svg-object')
    const svgDoc = svgObject.contentDocument
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
