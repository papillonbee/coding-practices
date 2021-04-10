package coding_practices.cache

import coding_practices.generator.Generator
import coding_practices.model.Dinosaur
import coding_practices.model.stubber.DinosaurStubber
import org.mockito.Matchers.any
import org.mockito.Mockito.{never, times, verify, when}
import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar

import java.util.UUID

class LeastRecentlyUsedCacheTest extends AnyFunSpec
  with Matchers
  with OptionValues
  with MockitoSugar {

  trait DummyDinosaurService {
    def fetchDinosaurById(id: UUID): Option[Dinosaur]
  }

  describe("LeastRecentlyUsedMemoryCache") {
    val maximumSize: Int = 3
    val lruCache: LeastRecentlyUsedCache[UUID, Dinosaur] =
      new LeastRecentlyUsedCacheImpl[UUID, Dinosaur](maximumSize)

    val dinosaurService: DummyDinosaurService = mock[DummyDinosaurService]

    it("should get value by key from cache if exist") {
      val dinosaurId: UUID = Generator.uuid.build
      val dinosaur: Dinosaur = DinosaurStubber.builder
        .name("John")
        .age(22)
        .build

      lruCache.put(dinosaurId, dinosaur)

      val cachedDinosaur: Option[Dinosaur] = lruCache.get(
        dinosaurId, () => dinosaurService.fetchDinosaurById(dinosaurId)
      )

      cachedDinosaur.value shouldEqual dinosaur
      verify(dinosaurService, never()).fetchDinosaurById(any[UUID])
    }

    it("should get value by key from the supplied callable if not exist in the cache") {
      val dinosaurId: UUID = Generator.uuid.build
      val dinosaur: Dinosaur = DinosaurStubber.builder
        .name("John")
        .age(22)
        .build

      val fetchedDinosaur: Option[Dinosaur] = Generator.option(dinosaur).build
      when(dinosaurService.fetchDinosaurById(dinosaurId)).thenReturn(fetchedDinosaur)

      val cachedDinosaur: Option[Dinosaur] = lruCache.get(
        dinosaurId, () => dinosaurService.fetchDinosaurById(dinosaurId)
      )

      cachedDinosaur shouldEqual fetchedDinosaur
      verify(dinosaurService, times(1)).fetchDinosaurById(dinosaurId)
    }

    it("should evict least recently used value when the configured maximum size is reached") {
      val numberOfDinosaurs: Int = maximumSize + 1
      val dinosaurIds: Seq[UUID] = Generator.uuid.buildList(numberOfDinosaurs)
      val dinosaurs: Seq[Dinosaur] = DinosaurStubber.buildList(numberOfDinosaurs)

      val dinosaurIdsZipWithDinosaursZipWithIndex: Seq[((UUID, Dinosaur), Int)] =
        (dinosaurIds zip dinosaurs).zipWithIndex

      dinosaurIdsZipWithDinosaursZipWithIndex foreach { case ((dinosaurId: UUID, dinosaur: Dinosaur), i: Int) =>
        lruCache.put(dinosaurId, dinosaur)
        when(dinosaurService.fetchDinosaurById(dinosaurId))
          .thenReturn(Generator.option(dinosaur,  nonNullProbability = 1).build)

        if (i == dinosaurIds.indices.last - 1) {
          lruCache.get(dinosaurIds.head, () => dinosaurService.fetchDinosaurById(dinosaurIds.head))
        }
      }

      val notEvictedFromCacheDinosaurs: Seq[((UUID, Dinosaur), Int)] =
        dinosaurIdsZipWithDinosaursZipWithIndex.filterNot(_._2 == dinosaurIds.indices.head + 1)

      notEvictedFromCacheDinosaurs foreach { case ((dinosaurId: UUID, dinosaur: Dinosaur), _: Int) =>
        val cachedDinosaur: Option[Dinosaur] = lruCache.get(
          dinosaurId, () => dinosaurService.fetchDinosaurById(dinosaurId)
        )

        cachedDinosaur.value shouldEqual dinosaur
        verify(dinosaurService, never()).fetchDinosaurById(dinosaurId)
      }

      val evictedFromCacheDinosaur: Seq[((UUID, Dinosaur), Int)] =
        dinosaurIdsZipWithDinosaursZipWithIndex.filter(_._2 == dinosaurIds.indices.head + 1)

      evictedFromCacheDinosaur foreach { case ((dinosaurId: UUID, dinosaur: Dinosaur), _: Int) =>
        val cachedDinosaur: Option[Dinosaur] = lruCache.get(
          dinosaurId, () => dinosaurService.fetchDinosaurById(dinosaurId)
        )

        cachedDinosaur.value shouldEqual dinosaur
        verify(dinosaurService, times(1)).fetchDinosaurById(dinosaurId)
      }
    }
  }
}
