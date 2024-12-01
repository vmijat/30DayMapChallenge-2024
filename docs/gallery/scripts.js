  
  document.addEventListener('DOMContentLoaded', () => {
    const gallery = document.getElementById('gallery');
    const lightbox = document.getElementById('lightbox');
    const lightboxImg = document.getElementById('lightbox-img');
    const lightboxTitle = document.getElementById('lightbox-title');
    const lightboxDescription = document.getElementById('lightbox-description');
    const closeBtn = document.querySelector('.close');
    const prevBtn = document.getElementById('prev-btn');
    const nextBtn = document.getElementById('next-btn');
    const toolsUsedContainer = document.getElementById('tools-used');
  
    let currentIndex = 0;
  
    // Render images in gallery using mapData
    mapData.forEach((map, index) => {
      const img = document.createElement('img');
      img.src = map.src;
      img.alt = map.alt;
      img.dataset.index = index;
      img.classList.add('gallery-img');
      gallery.appendChild(img);
  
      // Attach click event for lightbox view
      img.addEventListener('click', () => {
        lightbox.style.display = 'flex';
        lightboxImg.src = map.src;
        lightboxImg.alt = map.alt;
        lightboxTitle.innerText = map.title;
        lightboxDescription.innerHTML = map.description;
        currentIndex = index;
        updateToolsUsed(map.tools);
      });
    });
  
    // Close the lightbox
    closeBtn.addEventListener('click', () => {
      lightbox.style.display = 'none';
    });
  
    // Navigate images with arrow keys
    document.addEventListener('keydown', (e) => {
      if (lightbox.style.display === 'flex') {
        if (e.key === 'ArrowRight') {
          navigateNext();
        } else if (e.key === 'ArrowLeft') {
          navigatePrev();
        } else if (e.key === 'Escape') {
          lightbox.style.display = 'none';
        }
      }
    });
  
    // Navigate images with buttons
    prevBtn.addEventListener('click', navigatePrev);
    nextBtn.addEventListener('click', navigateNext);
  
    function navigateNext() {
      currentIndex = (currentIndex + 1) % mapData.length;
      updateLightbox(currentIndex);
    }
  
    function navigatePrev() {
      currentIndex = (currentIndex - 1 + mapData.length) % mapData.length;
      updateLightbox(currentIndex);
    }
  
    function updateLightbox(index) {
      const map = mapData[index];
      lightboxImg.src = map.src;
      lightboxImg.alt = map.alt;
      lightboxTitle.innerText = map.title;
      lightboxDescription.innerText = map.description;
      updateToolsUsed(map.tools);
    }
  
    // Function to dynamically update tools used section
    function updateToolsUsed(tools) {
      toolsUsedContainer.innerHTML = '<div class="tools-list"></div>';
      const toolsList = toolsUsedContainer.querySelector('.tools-list');
  
      for (let tool in tools) {
        const toolItem = document.createElement('div');
        toolItem.classList.add('tool-item');
  
        const toolIcon = document.createElement('span');
        toolIcon.classList.add('tool-icon', tools[tool] ? 'filled' : 'hollow');
        toolIcon.innerHTML = tools[tool] ? '&#9635;' : '&#9634;';
  
        toolItem.appendChild(toolIcon);
        toolItem.append(` ${tool}`);
        toolsList.appendChild(toolItem);
      }
    }
  });
  